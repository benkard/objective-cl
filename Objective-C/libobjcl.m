/* -*- mode: objc; coding: utf-8 -*- */

#import "libobjcl.h"
#import "libffi_support.h"
#import "objc_support.h"

#import <Foundation/Foundation.h>
#include <stdarg.h>
#include <objc/objc-api.h>

#ifdef __NEXT_RUNTIME__
#include <objc/objc-class.h>
#endif


static NSAutoreleasePool *objcl_autorelease_pool = NULL;

/* Preallocate an exception to throw when memory is all used up. */
NSException *objcl_oom_exception;


void
objcl_initialise_runtime (void)
{
  objcl_autorelease_pool = [[NSAutoreleasePool alloc] init];
  objcl_oom_exception = [NSException exceptionWithName: @"MLKOutOfMemoryException"
                                     reason: @"Out of memory"
                                     userInfo: nil];
}


void
objcl_shutdown_runtime (void)
{
  [objcl_autorelease_pool release];
}


#ifdef __NEXT_RUNTIME__
size_t
objc_sizeof_type (const char *typespec)
{
  switch (typespec[0])
    {
    case '@': return sizeof (id);
    case '#': return sizeof (Class);
    case ':': return sizeof (SEL);
    case 'c': return sizeof (char);
    case 'C': return sizeof (unsigned char);
    case 's': return sizeof (short);
    case 'S': return sizeof (unsigned short);
    case 'i': return sizeof (int);
    case 'I': return sizeof (unsigned int);
    case 'l': return sizeof (long);
    case 'L': return sizeof (unsigned long);
    case 'q': return sizeof (long long);
    case 'Q': return sizeof (unsigned long long);
    case 'f': return sizeof (float);
    case 'd': return sizeof (double);
    case 'B': return sizeof (BOOL);
    case '?':
    case '^': return sizeof (void *);
    case '*': return sizeof (char *);
    default:
      NSLog (@"Dammit.  What the heck is `%s' supposed to mean?",
             typespec);
      return 0;  /* FIXME: Should signal an error. */
    }
}
#endif


static void
_objcl_get_arg_pointer (void *buffer, OBJCL_OBJ_DATA argdata)
{
  void *source = NULL;

  switch (argdata->type[0])
    {
    case '@': source = &argdata->data.id_val; break;
    case '#': source = &argdata->data.id_val; break;
    case ':': source = &argdata->data.sel_val; break;
    case 'c': source = &argdata->data.char_val; break;
    case 'C': source = &argdata->data.char_val; break;
    case 's': source = &argdata->data.short_val; break;
    case 'S': source = &argdata->data.short_val; break;
    case 'i': source = &argdata->data.int_val; break;
    case 'I': source = &argdata->data.int_val; break;
    case 'l': source = &argdata->data.long_val; break;
    case 'L': source = &argdata->data.long_val; break;
    case 'q': source = &argdata->data.long_long_val; break;
    case 'Q': source = &argdata->data.long_long_val; break;
    case 'f': source = &argdata->data.float_val; break;
    case 'd': source = &argdata->data.double_val; break;
    case 'B': source = &argdata->data.bool_val; break;
/*    _OBJCL_ARG_CASE(_C_PTR, ptr); */
    case '^': source = &argdata->data.ptr_val; break;
    case '?': source = &argdata->data.ptr_val; break;
    case '*': source = &argdata->data.charptr_val; break;
/*
  case 'v': source = &argdata->data.oid_val; break;
  case 'b': source = &argdata->data.bitfield_val; break;
      _OBJCL_ARG_CASE(_C_ATOM, atom);
      _OBJCL_ARG_CASE(_C_ARY_B, );
      _OBJCL_ARG_CASE(_C_UNION_B, );
      _OBJCL_ARG_CASE(_C_STRUCT_B, );
      _OBJCL_ARG_CASE(_C_VECTOR, );
      _OBJCL_ARG_CASE(_C_COMPLEX, );
*/
/*  case '?': */
    default:
      NSLog (@"Dammit.  What the heck is `%s' supposed to mean?",
             argdata->type);
      return;  /* FIXME: Should signal an error. */
    }

  memmove (buffer, source, objc_sizeof_type (argdata->type));
}


static void
_objcl_invoke_method (id self_,
                      OBJCL_OBJ_DATA result,
                      NSMethodSignature *signature,
                      SEL selector,
                      int argc,
                      va_list arglist)
{
  int i;
  NSInvocation *invocation;
  void *result_ptr = NULL;
  const char *type = [signature methodReturnType];

  result->type = malloc (strlen (type) + 1);
  strcpy (result->type, type);

  if (signature == NULL)
    {
      [[NSException exceptionWithName: @"MLKNoSignatureFoundException"
                    reason: @"No signature found"
                    userInfo: NULL] raise];
    }

  
  switch (type[0])
    {
    case '@': result_ptr = &(result->data.id_val); break;
    case '#': result_ptr = &result->data.id_val; break;
    case ':': result_ptr = &result->data.sel_val; break;
    case 'c': result_ptr = &result->data.char_val; break;
    case 'C': result_ptr = &result->data.char_val; break;
    case 's': result_ptr = &result->data.short_val; break;
    case 'S': result_ptr = &result->data.short_val; break;
    case 'i': result_ptr = &result->data.int_val; break;
    case 'I': result_ptr = &result->data.int_val; break;
    case 'l': result_ptr = &result->data.long_val; break;
    case 'L': result_ptr = &result->data.long_val; break;
    case 'q': result_ptr = &result->data.long_long_val; break;
    case 'Q': result_ptr = &result->data.long_long_val; break;
    case 'f': result_ptr = &result->data.float_val; break;
    case 'd': result_ptr = &result->data.double_val; break;
    case 'B': result_ptr = &result->data.bool_val; break;
    case '?':
    case '^': result_ptr = &result->data.ptr_val; break;
    case '*': result_ptr = &result->data.charptr_val; break;
      /*
    case _C_BFLD: result_ptr = &result->data._val; break;
    case _C_VOID: result_ptr = &result->data._val; break;
    case _C_UNDEF: result_ptr = &result->data._val; break;
    case _C_ATOM: result_ptr = &result->data._val; break;
    case _C_ARY_B: result_ptr = &result->data._val; break;
    case _C_ARY_E: result_ptr = &result->data._val; break;
    case _C_UNION_B: result_ptr = &result->data._val; break;
    case _C_UNION_E: result_ptr = &result->data._val; break;
    case _C_STRUCT_B: result_ptr = &result->data._val; break;
    case _C_STRUCT_E: result_ptr = &result->data._val; break;
    case _C_VECTOR: result_ptr = &result->data._val; break;
    case _C_COMPLEX: result_ptr = &result->data._val; break;
      */
    }

  invocation = [NSInvocation invocationWithMethodSignature: signature];
  [invocation setTarget: self_];
  [invocation setSelector: selector];

  for (i = 0; i < argc; i++)
    {
      const char* type = [signature getArgumentTypeAtIndex: (i + 2)];
      void *buffer = malloc (objc_sizeof_type (type));
      OBJCL_OBJ_DATA arg = va_arg (arglist, OBJCL_OBJ_DATA);
      _objcl_get_arg_pointer (buffer, arg);

      if (type[0] == '#')
        NSLog (@"Argument %d: %@ (type %s)", i, buffer, type);
      else
        NSLog (@"Argument %d: type %s.", i, type);

      [invocation setArgument: buffer
                  atIndex: (i + 2)];

      free (buffer);
    }

  [invocation retainArguments];
  NSLog (@"Invoking %@ on %@.", invocation, self_);
  [invocation invoke];
  NSLog (@"Fetching return value.");
  [invocation getReturnValue: result_ptr];
  if (result->type[0] == '#')
    NSLog (@"Returning: %@", result->data.id_val);
  else
    NSLog (@"Returning.");
}


OBJCL_OBJ_DATA
objcl_invoke_method (OBJCL_OBJ_DATA receiver,
                     SEL method_selector,
                     int argc,
                     ...)
{
  va_list arglist;
  id self_ = NULL;
  NSMethodSignature *signature;
  OBJCL_OBJ_DATA result = malloc (sizeof (struct objcl_object));

  NS_DURING
    {
      switch (receiver->type[0])
        {
        case '#':
          self_ = receiver->data.class_val;
          break;
        case '@':
          self_ = receiver->data.id_val;
          break;
        case 'E':
          self_ = receiver->data.exc_val;
          break;
        default:
          return NULL;
        }


      signature = [self_ methodSignatureForSelector: method_selector];

      va_start (arglist, argc);
      _objcl_invoke_method (self_,
                            result,
                            signature,
                            method_selector,
                            argc,
                            arglist);
      va_end (arglist);
    }
  NS_HANDLER
    {
      result->type = malloc (strlen (EXCEPTION_TYPESPEC) + 1);
      strcpy (result->type, EXCEPTION_TYPESPEC);
      result->data.exc_val = localException;
      NS_VALUERETURN (result, void *);
    }
  NS_ENDHANDLER

    return result;
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
  return NSClassFromString ([NSString stringWithUTF8String: class_name]);
}


SEL
objcl_find_selector (const char *class_name)
{
  return NSSelectorFromString ([NSString stringWithUTF8String: class_name]);
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
objcl_get_yes ()
{
  if (sizeof (YES) > sizeof (long))
    fprintf (stderr, "WARNING: objcl_get_yes: YES might not fit into a long.\n");
  return YES;
}


long
objcl_get_no ()
{
  if (sizeof (NO) > sizeof (long))
    fprintf (stderr, "WARNING: objcl_get_no: NO might not fit into a long.\n");
  return NO;
}
