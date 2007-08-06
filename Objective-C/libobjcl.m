/* -*- mode: objc; coding: utf-8 -*- */

#import "libobjcl.h"
#import "Foundation/Foundation.h"
#include <stdarg.h>
#include <objc/objc-api.h>


static NSAutoreleasePool *objcl_autorelease_pool = NULL;


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


#define _OBJCL_ARG_CASE(typespec, field_name)                           \
  case typespec:                                                        \
  memmove (buffer, &argdata->data.field_name##_val,                     \
           objc_sizeof_type (argdata->type));                           \
  break;


static void
_objcl_get_arg_pointer (void *buffer, OBJCL_OBJ_DATA argdata)
{
  switch (argdata->type[0])
    {
      _OBJCL_ARG_CASE(_C_ID, id);
      _OBJCL_ARG_CASE(_C_CLASS, id);
      _OBJCL_ARG_CASE(_C_SEL, sel);
      _OBJCL_ARG_CASE(_C_CHR, char);
      _OBJCL_ARG_CASE(_C_UCHR, char);
      _OBJCL_ARG_CASE(_C_SHT, short);
      _OBJCL_ARG_CASE(_C_USHT, short);
      _OBJCL_ARG_CASE(_C_INT, int);
      _OBJCL_ARG_CASE(_C_UINT, int);
      _OBJCL_ARG_CASE(_C_LNG, long);
      _OBJCL_ARG_CASE(_C_ULNG, long);
      _OBJCL_ARG_CASE(_C_LNG_LNG, long_long);
      _OBJCL_ARG_CASE(_C_ULNG_LNG, long_long);
      _OBJCL_ARG_CASE(_C_FLT, float);
      _OBJCL_ARG_CASE(_C_DBL, double);
      _OBJCL_ARG_CASE(_C_BOOL, bool);
      _OBJCL_ARG_CASE(_C_PTR, ptr);
      _OBJCL_ARG_CASE(_C_CHARPTR, charptr);
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
      NSLog (@"Dammit.  What the heck is `%s' supposed to mean?",
             argdata->type);
      break;
    }
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
    case _C_ID: result_ptr = &(result->data.id_val); break;
    case _C_CLASS: result_ptr = &result->data.id_val; break;
    case _C_SEL: result_ptr = &result->data.sel_val; break;
    case _C_CHR: result_ptr = &result->data.char_val; break;
    case _C_UCHR: result_ptr = &result->data.char_val; break;
    case _C_SHT: result_ptr = &result->data.short_val; break;
    case _C_USHT: result_ptr = &result->data.short_val; break;
    case _C_INT: result_ptr = &result->data.int_val; break;
    case _C_UINT: result_ptr = &result->data.int_val; break;
    case _C_LNG: result_ptr = &result->data.long_val; break;
    case _C_ULNG: result_ptr = &result->data.long_val; break;
    case _C_LNG_LNG: result_ptr = &result->data.long_long_val; break;
    case _C_ULNG_LNG: result_ptr = &result->data.long_long_val; break;
    case _C_FLT: result_ptr = &result->data.float_val; break;
    case _C_DBL: result_ptr = &result->data.double_val; break;
    case _C_BOOL: result_ptr = &result->data.bool_val; break;
    case _C_PTR: result_ptr = &result->data.ptr_val; break;
    case _C_CHARPTR: result_ptr = &result->data.charptr_val; break;
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
}


OBJCL_OBJ_DATA
objcl_invoke_instance_method (OBJCL_OBJ_DATA receiver,
                              const char *method_name,
                              int argc,
                              ...)
{
  va_list arglist;
  id self_ = NULL;
  SEL selector;
  NSMethodSignature *signature;
  OBJCL_OBJ_DATA result = malloc (sizeof (struct objcl_object));

  NS_DURING
    {
      /* fprintf (stderr, "! ---------> %s <--------\n", receiver->type); */
      assert (receiver->type[0] == '#'
              || receiver->type[0] == '@'
              || receiver->type[0] == 'E');
      switch (receiver->type[0])
        {
        case '#': self_ = receiver->data.class_val;
        case '@': self_ = receiver->data.id_val;
        case 'E': self_ = receiver->data.exc_val;                
        }

      selector = NSSelectorFromString ([NSString
                                         stringWithUTF8String: method_name]);

      signature = [self_ instanceMethodSignatureForSelector: selector];

      va_start (arglist, argc);
      _objcl_invoke_method (self_, result, signature, selector, argc, arglist);
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


OBJCL_OBJ_DATA
objcl_invoke_class_method (OBJCL_OBJ_DATA class,
                           const char *method_name,
                           int argc,
                           ...)
{
  va_list arglist;
  id self_ = NULL;
  SEL selector;
  NSMethodSignature *signature;
  OBJCL_OBJ_DATA result = malloc (sizeof (struct objcl_object));

  NS_DURING
    {
      /* fprintf (stderr, "? ---------> %s <--------\n", class->type); */
      assert (class->type[0] == '#'
              || class->type[0] == '@'
              || class->type[0] == 'E');
      switch (class->type[0])
        {
        case '#': self_ = class->data.class_val;
        case '@': self_ = class->data.id_val;
        case 'E': self_ = class->data.exc_val;
        }

      selector = NSSelectorFromString ([NSString
                                         stringWithUTF8String: method_name]);

      signature = [self_ methodSignatureForSelector: selector];

      va_start (arglist, argc);
      _objcl_invoke_method (self_, result, signature, selector, argc, arglist);
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


OBJCL_OBJ_DATA
objcl_find_class (const char *class_name)
{
  Class class =
    NSClassFromString ([NSString stringWithUTF8String: class_name]);
  OBJCL_OBJ_DATA result = malloc (sizeof (struct objcl_object));
  const char *const typespec = "#8@0:4";

  result->type = malloc (strlen (typespec) + 1);
  strcpy (result->type, typespec);
  result->data.class_val = class;

  return result;
}


OBJCL_OBJ_DATA
objcl_find_selector (const char *class_name)
{
  SEL selector =
    NSSelectorFromString ([NSString stringWithUTF8String: class_name]);
  OBJCL_OBJ_DATA result = malloc (sizeof (struct objcl_object));
  const char *const typespec = ":";

  result->type = malloc (strlen (typespec) + 1);
  strcpy (result->type, typespec);
  result->data.sel_val = selector;

  return result;
}


const char *
objcl_class_name (OBJCL_OBJ_DATA class)
{
  Class cls = NULL;

  /* fprintf (stderr, "---------> %s <--------\n", class->type); */
  fflush (stderr);
  assert (class->type[0] == '#'
          || class->type[0] == '@'
          || class->type[0] == 'E');
  switch (class->type[0])
    {
    case '#': cls = class->data.class_val;
    case '@': cls = class->data.id_val;
    case 'E': cls = (id) class->data.exc_val;
    }

  return class_get_class_name (cls);
}
