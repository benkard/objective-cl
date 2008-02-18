/* -*- mode: objc; coding: utf-8 -*- */
/* Objective-CL, an Objective-C bridge for Common Lisp.
 * Copyright (C) 2007  Matthias Andreas Benkard.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#import "libobjcl.h"
#import "NSObject-ObjectiveCLWrapperLink.h"
#import "PyObjC/libffi_support.h"
#import "JIGS/ObjcRuntimeUtilities.h"

#import "Foundation/Foundation.h"

#include <stdarg.h>
#include <sys/mman.h>
#include <objc/objc-api.h>

#ifdef __NEXT_RUNTIME__
#include <objc/objc-class.h>
#endif

#if 0
#define TRACE NSLog
#else
#define TRACE objcl_null_log
#endif

static void
objcl_null_log (NSString *s, ...)
{
}


static NSAutoreleasePool *objcl_autorelease_pool = nil;

/* Preallocate an exception to throw when memory is all used up. */
NSException *objcl_oom_exception = nil;

id objcl_current_exception = nil;
NSRecursiveLock *objcl_current_exception_lock = nil;

static NSMutableDictionary *method_lists = nil;
static NSMutableDictionary *method_list_lengths = nil;

/* A class is considered Lisp-backed if some of its methods are
   implemented as Lisp callbacks.  This is true if and only if
   @selector(retain) and @selector(release) are overridden by
   Objective-CL.  In this case, the corresponding Lisp objects are
   stored in a regular hash table instead of a weak one, as they may
   hold data (like CLOS slots) that we can't do without as long as the
   Objective-C instance is referenced from anywhere (where `anywhere'
   includes both the Lisp and Objective-C worlds). */
static NSMutableSet *lisp_backed_classes = nil;

static int init_count = 0;


void
objcl_initialise_runtime (void)
{
  if (init_count <= 0)
    {
      objcl_autorelease_pool = [[NSAutoreleasePool alloc] init];
      objcl_oom_exception = [NSException exceptionWithName: @"MLKOutOfMemoryException"
                                         reason: @"Out of memory"
                                         userInfo: nil];
      [objcl_oom_exception retain];

#ifdef __NEXT_RUNTIME__
      PyObjC_SetupRuntimeCompat ();
#endif

      objcl_current_exception_lock = [[NSRecursiveLock alloc] init];
      method_lists = [[NSMutableDictionary alloc] init];
      method_list_lengths = [[NSMutableDictionary alloc] init];
      lisp_backed_classes = [[NSMutableSet alloc] init];
      init_count = 1;
    }
  else
    init_count++;
}


static void
release_unless_null (id *object)
{
  if (*object != nil)
    {
      [*object release];
      *object = nil;
    }
}


void
objcl_shutdown_runtime (void)
{
  init_count--;
  if (init_count == 0)
    {
      release_unless_null (&objcl_autorelease_pool);
      release_unless_null (&objcl_current_exception_lock);
      release_unless_null (&objcl_oom_exception);
      release_unless_null (&method_lists);
      release_unless_null (&method_list_lengths);
      release_unless_null (&lisp_backed_classes);
    }
  else if (init_count < 0)
    init_count = 0;
}


#ifdef USE_LIBFFI
id
objcl_invoke_with_types (int argc,
                         Class superclass_for_send_super,
                         char *return_typespec,
                         char *arg_typespecs[],
                         void *return_value,
                         void **argv)
{
  IMP method;
  int i;
  ffi_cif cif;
  ffi_type *return_type;
  ffi_type *arg_types[argc + 2];
  ffi_status status;

  id receiver = *((id*)argv[0]);
  SEL method_selector = *((SEL*)argv[1]);

  static ffi_type *id_type = NULL;
  static ffi_type *sel_type = NULL;

  if (!id_type)
    id_type = objcl_pyobjc_arg_signature_to_ffi_type ("@");
  if (!sel_type)
    sel_type = objcl_pyobjc_arg_signature_to_ffi_type (":");

  NS_DURING
    {
      TRACE (@"get-method");
      /* FIXME: The NeXT runtime wants to use special functions for
         structure and floating-point returns.

         Note that there is no objc_msgSendSuper_fpret.  The reason is
         that objc_msgSendSuper will never be passed nil as the instance
         to call a method on, while using objc_msgSend_fpret is
         important only so that sending a message to nil may return a
         sane value.

         Which means that if we don't allow nil to be messaged, we
         probably don't need to bother with objc_msgSend_fpret,
         either. */
      method = objcl_get_method_implementation (receiver, method_selector,
                                                superclass_for_send_super);
      TRACE (@"method == NULL");
      if (method == NULL)
        [[NSException exceptionWithName: @"MLKNoApplicableMethod"
                      reason: @"Tried to call a non-existent method."
                      userInfo: nil] raise];

      TRACE (@"return type");
      return_type = objcl_pyobjc_signature_to_ffi_return_type (return_typespec);
      arg_types[0] = id_type;
      arg_types[1] = sel_type;

      TRACE (@"args");
      for (i = 0; i < argc; i++)
        arg_types[i + 2] = objcl_pyobjc_arg_signature_to_ffi_type (arg_typespecs[i]);

      TRACE (@"prep");
      status = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, argc + 2, return_type, arg_types);
      if (status != FFI_OK)
        {
          [[NSException exceptionWithName: @"MLKInvalidFFITypeException"
                        reason: @"FFI type is invalid (this is probably a bug)."
                        userInfo: nil] raise];
        }

      TRACE (@"call");
      ffi_call (&cif, FFI_FN (method), return_value, argv);
      TRACE (@"...");
    }
  NS_HANDLER
    {
      NS_VALUERETURN (localException, id);
    }
  NS_ENDHANDLER

  return nil;
}
#endif


Class
objcl_find_class (const char *class_name)
{
  TRACE (@"find-class %s", class_name);
#ifdef __NEXT_RUNTIME__
  return objc_getClass (class_name);
#else
  return objc_lookup_class (class_name);
#endif
}


Class
objcl_find_meta_class (const char *class_name)
{
  TRACE (@"find-meta-class %s", class_name);
#ifdef __NEXT_RUNTIME__
  return objc_getMetaClass (class_name);
#else
  /* FIXME: Is this correct? */
  Class class = objcl_find_class (class_name);
  if (class == NULL || class == Nil)
    return Nil;
  else
    return class_get_meta_class (class);
#endif
}


SEL
objcl_find_selector (const char *selector_name)
{
#ifdef __NEXT_RUNTIME__
  if (!(sel_isMapped ((SEL) selector_name)))  /* XXX Does this work? */
    return NULL;
  else
    return sel_getUid (selector_name);
#else
  return sel_get_any_uid (selector_name);
#endif
}


SEL
objcl_intern_selector (const char *selector_name)
{
  /* sel_registerName and sel_register_name seem not to be necessary here. */
#ifdef __NEXT_RUNTIME__
  return sel_getUid (selector_name);
#else
  return sel_get_uid (selector_name);
#endif
}


const char *
objcl_class_name (Class class)
{
  const char *ns_name;
  char *name;

  TRACE (@"class-name");
  ns_name = [(NSStringFromClass (class)) UTF8String];
  name = malloc (strlen (ns_name) + 1);
  strcpy (name, ns_name);

  return name;
}


Class
objcl_class_superclass (Class class)
{
  TRACE (@"super-class");

  /* Not strictly needed on the GNU runtime, but not going to hurt
     anyone either. */
  if (class == [NSObject class])
    return nil;

#ifdef __NEXT_RUNTIME__
  return class_getSuperclass (class);
#else
  return class_get_super_class (class);
#endif
}


Class
objcl_class_metaclass (Class class)
{
#ifdef __NEXT_RUNTIME__
  return object_getClass (class);
#else
  return class_get_meta_class (class);
#endif
}


const char *
objcl_selector_name (SEL selector)
{
  const char *ns_name;
  char *name;

  ns_name = [(NSStringFromSelector (selector)) UTF8String];
  name = malloc (strlen (ns_name) + 1);
  strcpy (name, ns_name);

  return name;
}


IMP
objcl_get_method_implementation (id object,
                                 SEL selector,
                                 Class superclass_for_send_super)
{
  /* If superclass_for_send_super == nil, this is just plain old method
     implementation hunting.  If it isn't, though, we're trying to do a
     super call, which can get a bit hairy quickly. */
  TRACE (@"method-impl %p %p", object, selector);
#ifdef __NEXT_RUNTIME__
  Class target_class;

  if (objcl_object_is_class (object))
    {
      if (superclass_for_send_super == Nil)
        target_class = object;
      else
        target_class = superclass_for_send_super;

      return method_getImplementation (class_getClassMethod (target_class,
                                                             selector));
    }
  else
    {
      if (superclass_for_send_super == Nil)
        target_class = [object class];
      else
        target_class = superclass_for_send_super;

#ifdef __OBJC2__
      return class_getMethodImplementation (target_class, selector);
#else
      return method_getImplementation (class_getInstanceMethod (target_class,
                                                                selector));
#endif
    }
#else
  if (superclass_for_send_super == Nil)
    return objc_msg_lookup (object, selector);
  else
    {
      Super super_struct;
      super_struct.self = object;
      super_struct.class = superclass_for_send_super;
      return objc_msg_lookup_super (&super_struct, selector);
    }
#endif
}


BOOL
objcl_object_is_class (id obj)
{
  TRACE (@"is-class %p", obj);
#ifdef __NEXT_RUNTIME__
  return [obj class] == obj;
#else
  /* return CLS_ISCLASS (obj); */
  return object_is_class (obj);
#endif
}


BOOL
objcl_object_is_meta_class (id obj)
{
  TRACE (@"is-meta-class %p", obj);
#ifdef __NEXT_RUNTIME__
  return objcl_object_is_class (obj) && class_isMetaClass (obj);
#else
  /* return CLS_ISMETA (ptr); */
  if (objcl_object_is_class (obj))
    return class_is_meta_class (obj);
  else
    return object_is_meta_class (obj);
#endif
}


Class
objcl_object_get_class (id obj)
{
  TRACE (@"get-class %p", obj);
#ifdef __NEXT_RUNTIME__
  return object_getClass (obj);
#else
  return object_get_class (obj);
#endif
}


Class
objcl_object_get_meta_class (id obj)
{
  TRACE (@"get-meta-class %p", obj);
#ifdef __NEXT_RUNTIME__
  /* FIXME: What to do here? */
  return objc_getMetaClass ([(NSStringFromClass ([obj class])) UTF8String]);
#else
  if (objcl_object_is_class (obj))
    return class_get_meta_class (obj);
  else
    return object_get_meta_class (obj);
#endif
}


id
objcl_get_nil (void)
{
  return nil;
}


long
objcl_get_yes (void)
{
  if (sizeof (YES) > sizeof (long))
    fprintf (stderr, "WARNING: objcl_get_yes: YES might not fit into a long.\n");
  return YES;
}


long
objcl_get_no (void)
{
  if (sizeof (NO) > sizeof (long))
    fprintf (stderr, "WARNING: objcl_get_no: NO might not fit into a long.\n");
  return NO;
}


const char *
objcl_get_runtime_type (void)
{
#ifdef __NEXT_RUNTIME__
  return "NeXT";
#else
  return "GNU";
#endif
}


int
objcl_objc2_p (void)
{
#ifdef __OBJC2__
  return 1;
#else
  return 0;
#endif
}


long
objcl_sizeof_type (const char *typespec)
{
  if (sizeof (ssize_t) > sizeof (long))
    fprintf (stderr, "WARNING: objcl_sizeof_typespec: Size might not fit into a long.\n");
  return PyObjCRT_SizeOfType (typespec);
}


long
objcl_sizeof_return_type (const char *typespec)
{
  if (sizeof (ssize_t) > sizeof (long))
    fprintf (stderr, "WARNING: objcl_sizeof_return_typespec: Size might not fit into a long.\n");
  return PyObjCRT_SizeOfReturnType (typespec);
}


long
objcl_alignof_type (const char *typespec)
{
  if (sizeof (ssize_t) > sizeof (long))
    fprintf (stderr, "WARNING: objcl_align_typespec: Alignment might not fit into a long.\n");
  return PyObjCRT_AlignOfType (typespec);
}


void
objcl_set_slot_value (id obj, const char *ivar_name, void *value)
{
  /* For the GNU runtime, this function is defined in objc-runtime-gnu.m. */
  object_setInstanceVariable (obj, ivar_name, value);
}


void
objcl_get_slot_value (id obj, const char *ivar_name, void *value_out)
{
  /* Caching Ivars may be useful here.  Using those instead of strings
     is claimed to be faster. */

  /* For the GNU runtime, this function is defined in objc-runtime-gnu.m. */

  /* NOTE: Contrary to what the official Objective-C runtime docs claim,
     value_out is actually a (void *) rather than a (void **).
     Likewise, the result that is copied to value_out is the slot value
     itself, not a pointer to it. */

  /* NOTE UPDATE: Actually, it's trickier than that.  The docs for
     NeXTstep 3.3 say: ``These functions cannot reliably be used to set
     and get instance variables that are not pointers.''  This makes the
     behaviour and documentation a bit less confusing, because it means
     that value_out is, in fact, assigned a pointer to the value of the
     slot under the assumption that the slot itself references its value
     via a pointer. */
  object_getInstanceVariable (obj, ivar_name, value_out);
}


IVAR_T *
objcl_class_direct_slots (Class class, unsigned int *count, unsigned int *element_size)
{
  IVAR_T *ivars;

#ifdef __NEXT_RUNTIME__
  TRACE (@"slots");
#else
  int i;
#endif

  *element_size = sizeof (IVAR_T);

#ifdef __NEXT_RUNTIME__
  ivars = class_copyIvarList (class, count);
#else
  *count = (class->ivars ? class->ivars->ivar_count : 0);
  if (!*count)
    ivars = NULL;
  else
    {
      ivars = malloc ((*count) * (*element_size));
      for (i = 0; i < *count; i++)
        ivars[i] = &class->ivars->ivar_list[i];
    }
#endif

  return ivars;
}


const char *
objcl_slot_name (IVAR_T ivar)
{
  TRACE (@"slot-name");
#ifdef __NEXT_RUNTIME__
  return ivar_getName (ivar);
#else
  return ivar->ivar_name;
#endif
}


const char *
objcl_slot_type (IVAR_T ivar)
{
#ifdef __NEXT_RUNTIME__
  return ivar_getTypeEncoding (ivar);
#else
  return ivar->ivar_type;
#endif
}


/* In order to be able to do exception propagation from Lisp code, we
   have the Lisp layer save exceptions to objcl_current_exception.  Our
   wrapper function is then able to raise the exception from where it
   ought to be raised from: the Objective-C layer.

   Note that it is the Lisp layer's duty to wrap Objective-C exceptions
   around Lisp SERIOUS-CONDITIONs in order to propagate those. */
static void
imp_closure (ffi_cif *cif, void *result, void **args, void *user_data)
{
  id exception;

  ffi_call (cif, user_data, result, args);

  exception = objcl_current_exception;
  objcl_current_exception = nil;
  objcl_release_lock (objcl_current_exception_lock);

  if (exception != nil)
    [exception raise];
}


IMP
objcl_create_imp (IMP callback,
                  int argc,
                  const char *return_typespec,
                  const char *arg_typespecs[])
{
  ffi_type *return_type;
  ffi_type *arg_types[argc + 2];
  ffi_status status;
  ffi_cif cif;
  ffi_closure *closure;

  int i;

  static ffi_type *id_type = NULL;
  static ffi_type *sel_type = NULL;

  if (!id_type)
    id_type = objcl_pyobjc_arg_signature_to_ffi_type ("@");

  if (!sel_type)
    sel_type = objcl_pyobjc_arg_signature_to_ffi_type (":");

  return_type = objcl_pyobjc_signature_to_ffi_return_type (return_typespec);

  arg_types[0] = id_type;
  arg_types[1] = sel_type;

  for (i = 0; i < argc; i++)
    arg_types[i + 2] = objcl_pyobjc_arg_signature_to_ffi_type (arg_typespecs[i]);

  status = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, argc + 2, return_type, arg_types);
  if (status != FFI_OK)
    {
      [[NSException exceptionWithName: @"MLKInvalidFFITypeException"
                    reason: @"FFI type is invalid (this is probably a bug)."
                    userInfo: nil] raise];
    }

  status = ffi_prep_closure (closure, &cif, imp_closure, (void *)callback);
  if (status != FFI_OK)
    {
      [[NSException exceptionWithName: @"MLKClosureCreationFailure"
                    reason: @"Creating an IMP closure failed (this is probably a bug)."
                    userInfo: nil] raise];
    }

  if (mprotect (closure, sizeof (closure), PROT_READ | PROT_EXEC) == -1)
    {
      [[NSException exceptionWithName: @"MLKClosureCreationFailure"
                    reason: @"Creating an IMP closure failed (this is probably a bug)."
                    userInfo: nil] raise];
    }

  return (IMP) closure;
}


void
objcl_acquire_lock (id lock)
{
  [lock lock];
  TRACE (@"Lock %@ acquired.", lock);
}


void
objcl_release_lock (id lock)
{
  [lock unlock];
  TRACE (@"Lock %@ released.", lock);
}


Class
objcl_create_class (const char *class_name,
                    Class superclass,
                    int protocol_number,
                    const char *protocol_names[],
                    int ivar_number,
                    const char *ivar_names[],
                    const char *ivar_typespecs[])
{
#ifdef __NEXT_RUNTIME__
  int i;
  Class class;

  class = objc_allocateClassPair (superclass, class_name, 0);

  for (i = 0; i < ivar_number; i++)
    preclass_addIvar (class,
                      ivar_names[i],
                      objcl_sizeof_type (ivar_typespecs[i]),
                      objcl_alignof_type (ivar_typespecs[i]),
                      ivar_typespecs[i]);

#ifdef __OBJC2__
  /* FIXME: What to do for the NeXT Objective-C 1.0 and GNU runtimes
     here? */
  for (i = 0; i < protocol_number; i++)
    preclass_addProtocol (class,
                          objc_getProtocol ((char *) protocol_names[i])
                          /* ??? !__OBJC2__ ???
                             objc_getClass (protocol_names[i]) */
                          );
#endif

  return class;
#else
  ffi_cif cif;
  ffi_status status;
  ffi_type *arg_types[3 + ivar_number*2];
  void *argv[3 + ivar_number*2];
  int i;
  BOOL return_value;
  const char *superclass_name;

  arg_types[0] = &ffi_type_pointer;
  arg_types[1] = &ffi_type_pointer;
  arg_types[2] = &ffi_type_sint;
  for (i = 0; i < ivar_number*2; i++)
    arg_types[3 + i] = &ffi_type_pointer;

  superclass_name = objcl_class_name (superclass);

  argv[0] = &class_name;
  argv[1] = &superclass_name;
  argv[2] = &ivar_number;
  for (i = 0; i < ivar_number; i++)
    {
      argv[3 + 2*i] = (void *) &ivar_names[i];
      argv[3 + 2*i + 1] = (void *) &ivar_typespecs[i];
    }

  TRACE (@"Arg 0: %s", *((char **) argv[0]));
  TRACE (@"Arg 1: %s", *((char **) argv[1]));
  TRACE (@"Arg 2: %d", *((int *) argv[2]));
  for (i = 3; i < 3 + 2*ivar_number; i++)
    {
      TRACE (@"Arg %d: %s", i, *((char **) argv[i]));
    }

  status = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, ivar_number*2 + 3, &ffi_type_uchar, arg_types);
  if (status != FFI_OK)
    {
      [[NSException exceptionWithName: @"MLKInvalidFFITypeException"
                    reason: @"FFI type is invalid (this is probably a bug)."
                    userInfo: nil] raise];
    }

  TRACE (@"ObjcUtilities_new_class");
  ffi_call (&cif, FFI_FN (ObjcUtilities_new_class), &return_value, argv);
  TRACE (@"ObjcUtilities_new_class end");

  NSString *ns_class_name = [NSString stringWithUTF8String: class_name];
  [method_lists setObject: [NSValue valueWithPointer: nil]
                forKey: ns_class_name];
  [method_list_lengths setObject: [NSNumber numberWithInt: 0]
                       forKey: ns_class_name];

  return objcl_find_class (class_name);
#endif
}


void
objcl_add_method (Class class,
                  SEL method_name,
                  IMP callback,
                  int argc,
                  const char *return_typespec,
                  const char *arg_typespecs[],
                  const char *signature)
{
  IMP imp;

  imp = objcl_create_imp (callback, argc, return_typespec, arg_typespecs);

#ifdef __NEXT_RUNTIME__
  preclass_addMethod (class, method_name, imp, signature);
#else
  NSString *class_name;
  struct ObjCLMethod **methods;
  int index;

  class_name = [NSString stringWithUTF8String: objcl_class_name (class)];
  index = [[method_list_lengths objectForKey: class_name] intValue];
  methods = [[method_lists objectForKey: class_name] pointerValue];

  methods = realloc (methods, (index + 1) * sizeof (struct ObjCLMethod *));
  methods[index] = malloc (sizeof (struct ObjCLMethod));

  methods[index]->signature = malloc (strlen (signature) + 1);

  methods[index]->method_name = method_name;
  strcpy (methods[index]->signature, signature);
  methods[index]->imp = imp;

  [method_lists setObject: [NSValue valueWithPointer: methods]
                forKey: class_name];
  [method_list_lengths setObject: [NSNumber numberWithInt: (index + 1)]
                       forKey: class_name];
#endif
}


void
objcl_finalise_class (Class class)
{
#ifdef __NEXT_RUNTIME__
  /* FIXME: Should we do this if class is a metaclass? */
  if (!objcl_object_is_meta_class (class))
    objc_registerClassPair (class);
#else
  int i;
  int method_count;
  NSString *class_name;
  MethodList *method_list;
  struct ObjCLMethod **methods;

  class_name = [NSString stringWithUTF8String: objcl_class_name (class)];
  methods = [[method_lists objectForKey: class_name] pointerValue];

  if (methods)
    {
      method_list = ObjcUtilities_alloc_method_list (method_count);
      method_count = [[method_list_lengths objectForKey: class_name] intValue];

      for (i = 0; i < method_count; i++)
        {
          ObjcUtilities_insert_method_in_list
            (method_list,
             i,
             objcl_selector_name (methods[i]->method_name),
             ObjcUtilities_build_runtime_Objc_signature (methods[i]->signature),
             methods[i]->imp);

          free (methods[i]->signature);
          free (methods[i]);
        }

      free (methods);
      ObjcUtilities_register_method_list (class, method_list);
    }

  [method_lists removeObjectForKey: class_name];
  [method_list_lengths removeObjectForKey: class_name];
#endif
}


int
objcl_class_backed_by_lisp_class_p (Class class)
{
  return [lisp_backed_classes containsObject: class];
}


void
objcl_class_set_backed_by_lisp_class (Class class, int backed_p)
{
  if (backed_p)
    [lisp_backed_classes addObject: class];
  else
    [lisp_backed_classes removeObject: class];
}


int
objcl_object_backed_by_lisp_class_p (id object)
{
  return objcl_class_backed_by_lisp_class_p ([object class]);
}
