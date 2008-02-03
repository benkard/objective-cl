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
#import "PyObjC/libffi_support.h"
#import "PyObjC/objc_support.h"
#import "PyObjC/objc-runtime-compat.h"

#import <Foundation/Foundation.h>
#include <stdarg.h>
#include <objc/objc-api.h>

#ifdef __NEXT_RUNTIME__
#include <objc/objc-class.h>
#endif


static NSAutoreleasePool *objcl_autorelease_pool = NULL;

/* Preallocate an exception to throw when memory is all used up. */
NSException *objcl_oom_exception = NULL;


void
objcl_initialise_runtime (void)
{
  if (!objcl_autorelease_pool)
    objcl_autorelease_pool = [[NSAutoreleasePool alloc] init];
  if (!objcl_oom_exception)
    {
      objcl_oom_exception = [NSException exceptionWithName: @"MLKOutOfMemoryException"
                                         reason: @"Out of memory"
                                         userInfo: nil];
      [objcl_oom_exception retain];
    }
}


void
objcl_shutdown_runtime (void)
{
  if (objcl_autorelease_pool)
    {
      [objcl_autorelease_pool release];
      objcl_autorelease_pool = NULL;
    }
  if (objcl_oom_exception)
    {
      [objcl_oom_exception release];
      objcl_oom_exception = NULL;
    }
}


#ifdef USE_LIBFFI
id
objcl_invoke_with_types (int argc,
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
#ifdef __NEXT_RUNTIME__
      if (objcl_object_is_class (receiver))
        method = class_getClassMethod (receiver, method_selector)->method_imp;
      else
        method = class_getInstanceMethod ([receiver class], method_selector)->method_imp;
#else
      method = objc_msg_lookup (receiver, method_selector);
      /* Alternatively:
         method = [receiver methodForSelector: method_selector];
      */
#endif

      if (method == NULL)
        [[NSException exceptionWithName: @"MLKNoApplicableMethod"
                      reason: @"Tried to call a non-existent method."
                      userInfo: nil] raise];

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

      ffi_call (&cif, FFI_FN (method), return_value, argv);
    }
  NS_HANDLER
    {
      NS_VALUERETURN (localException, id);
    }
  NS_ENDHANDLER

  return NULL;
}
#endif


Class
objcl_find_class (const char *class_name)
{
#ifdef __NEXT_RUNTIME__
  return objc_lookUpClass (class_name);
#else
  return objc_lookup_class (class_name);
#endif
}


Class
objcl_find_meta_class (const char *class_name)
{
#ifdef __NEXT_RUNTIME__
  return objc_getMetaClass (class_name);
#else
  /* FIXME: Is this correct? */
  Class class = objcl_find_class (class_name);
  if (class == NULL || class == nil)
    return NULL;
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

  ns_name = [(NSStringFromClass (class)) UTF8String];
  name = malloc (strlen (ns_name) + 1);
  strcpy (name, ns_name);

  return name;
}


Class
objcl_class_superclass (Class class)
{
#ifdef __NEXT_RUNTIME__
  return class_getSuperclass (class);
#else
  return class_get_super_class (class);
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
                                 SEL selector)
{
#ifdef __NEXT_RUNTIME__
  if (objcl_object_is_class (object))
    {
      return class_getClassMethod (object, selector)->method_imp;
    }
  else
    {
      return class_getInstanceMethod ([object class], selector)->method_imp;
    }
#else
  return objc_msg_lookup (object, selector);
#endif
}


BOOL
objcl_object_is_class (id obj)
{
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
#ifdef __NEXT_RUNTIME__
  /* FIXME: What to do here? */
  return objcl_object_get_meta_class (obj) == obj;
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
#ifdef __NEXT_RUNTIME__
  /* XXX? return obj->isa; */
  return [obj class];
#else
  return object_get_class (obj);
#endif
}


Class
objcl_object_get_meta_class (id obj)
{
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
