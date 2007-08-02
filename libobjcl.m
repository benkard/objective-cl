/* -*- mode: objc; coding: utf-8 -*- */

#import "libobjcl.h"
#import "Foundation/Foundation.h"
#include <stdarg.h>
#include <objc/objc-api.h>


NSAutoreleasePool *objcl_autorelease_pool = NULL;


void
objcl_initialise_runtime ()
{
  objcl_autorelease_pool = [[NSAutoreleasePool alloc] init];
}


void
objcl_shutdown_runtime ()
{
  [objcl_autorelease_pool release];
}


#define _OBJCL_ARG_DECL(typespec, c_type)       \
  c_type __##typespec##_tmp


#define _OBJCL_ARG_CASE(typespec, c_type)                               \
  case typespec:                                                        \
  __##typespec##_tmp = va_arg (arglist, c_type);                        \
  memmove (buffer, &__##typespec##_tmp, objc_sizeof_type (type));       \
  break;


void
_objcl_get_arg_pointer (void *buffer, const char *type, va_list arglist)
{
  _OBJCL_ARG_DECL(_C_ID, id);
  _OBJCL_ARG_DECL(_C_CLASS, id);
  _OBJCL_ARG_DECL(_C_SEL, SEL);
  _OBJCL_ARG_DECL(_C_CHR, char);
  _OBJCL_ARG_DECL(_C_UCHR, unsigned char);
  _OBJCL_ARG_DECL(_C_SHT, short);
  _OBJCL_ARG_DECL(_C_USHT, unsigned short);
  _OBJCL_ARG_DECL(_C_INT, int);
  _OBJCL_ARG_DECL(_C_UINT, unsigned int);
  _OBJCL_ARG_DECL(_C_LNG, long);
  _OBJCL_ARG_DECL(_C_ULNG, unsigned long);
  _OBJCL_ARG_DECL(_C_LNG_LNG, long long);
  _OBJCL_ARG_DECL(_C_ULNG_LNG, unsigned long long);
  _OBJCL_ARG_DECL(_C_FLT, float);
  _OBJCL_ARG_DECL(_C_DBL, double);
  _OBJCL_ARG_DECL(_C_BOOL, BOOL);
  _OBJCL_ARG_DECL(_C_PTR, void *);
  _OBJCL_ARG_DECL(_C_CHARPTR, char *);
  
  switch (type[0])
    {
      _OBJCL_ARG_CASE(_C_ID, id);
      _OBJCL_ARG_CASE(_C_CLASS, id);
      _OBJCL_ARG_CASE(_C_SEL, SEL);
      _OBJCL_ARG_CASE(_C_CHR, int);
      _OBJCL_ARG_CASE(_C_UCHR, int);
      _OBJCL_ARG_CASE(_C_SHT, int);
      _OBJCL_ARG_CASE(_C_USHT, int);
      _OBJCL_ARG_CASE(_C_INT, int);
      _OBJCL_ARG_CASE(_C_UINT, unsigned int);
      _OBJCL_ARG_CASE(_C_LNG, long);
      _OBJCL_ARG_CASE(_C_ULNG, unsigned long);
      _OBJCL_ARG_CASE(_C_LNG_LNG, long long);
      _OBJCL_ARG_CASE(_C_ULNG_LNG, unsigned long long);
      _OBJCL_ARG_CASE(_C_FLT, double);
      _OBJCL_ARG_CASE(_C_DBL, double);
      _OBJCL_ARG_CASE(_C_BOOL, int);
      _OBJCL_ARG_CASE(_C_PTR, void *);
      _OBJCL_ARG_CASE(_C_CHARPTR, char *);
/*
      _OBJCL_ARG_CASE(_C_VOID, void);
      _OBJCL_ARG_CASE(_C_BFLD, bitfield);
      _OBJCL_ARG_CASE(_C_ATOM, atom);
      _OBJCL_ARG_CASE(_C_ARY_B, );
      _OBJCL_ARG_CASE(_C_UNION_B, );
      _OBJCL_ARG_CASE(_C_STRUCT_B, );
      _OBJCL_ARG_CASE(_C_VECTOR, );
      _OBJCL_ARG_CASE(_C_COMPLEX, );
*/
    case _C_UNDEF:
    default:
      NSLog (@"Dammit.  What the heck is `%s' supposed to mean?", type);
      break;
    }
}


void *
_objcl_invoke_method (id self_,
                      NSMethodSignature *signature,
                      SEL selector,
                      int argc,
                      va_list arglist)
{
  int i;
  NSInvocation *invocation;
  id result = NULL;

  if (signature == NULL)
    {
      return NULL;
    }

  invocation = [NSInvocation invocationWithMethodSignature: signature];
  [invocation setTarget: self_];
  [invocation setSelector: selector];

  for (i = 0; i < argc; i++)
    {
      const char* type = [signature getArgumentTypeAtIndex: (i + 2)];
      NSLog (@"Argument %d: type %s.", i, type);

      void *buffer = malloc (objc_sizeof_type (type));
      _objcl_get_arg_pointer (buffer, type, arglist);

      [invocation setArgument: buffer
                  atIndex: (i + 2)];

      free (buffer);
    }

  [invocation retainArguments];
  NSLog (@"Invoking %@ on %@.", invocation, self_);
  [invocation invoke];
  NSLog (@"Fetching return value.");
  [invocation getReturnValue: &result];
  NSLog (@"Returning: %@", result);

  return result;
}


void *
objcl_invoke_instance_method (void *receiver,
                              char *const method_name,
                              int argc,
                              ...)
{
  va_list arglist;
  id self_;
  SEL selector;
  NSMethodSignature *signature;
  void *result;

  self_ = (id) receiver;
  selector = NSSelectorFromString ([NSString
                                     stringWithUTF8String: method_name]);

  signature = [self_ instanceMethodSignatureForSelector: selector];

  va_start (arglist, argc);
  result = _objcl_invoke_method (self_, signature, selector, argc, arglist);
  va_end (arglist);

  return result;
}


void *
objcl_invoke_class_method (void *class,
                           char *const method_name,
                           int argc,
                           ...)
{
  va_list arglist;
  id self_;
  SEL selector;
  NSMethodSignature *signature;
  void *result;

  self_ = (id) class;
  selector = NSSelectorFromString ([NSString
                                     stringWithUTF8String: method_name]);

  signature = [self_ methodSignatureForSelector: selector];

  va_start (arglist, argc);
  result = _objcl_invoke_method (self_, signature, selector, argc, arglist);
  va_end (arglist);

  return result;
}


void *
objcl_find_class (char *const class_name)
{
  return NSClassFromString ([NSString stringWithUTF8String: class_name]);
}

