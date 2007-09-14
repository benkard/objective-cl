/* -*- mode: objc; coding: utf-8 -*- */

#import "libobjcl.h"
#import "Foundation/Foundation.h"
#include <stdarg.h>
#include <objc/objc-api.h>

#ifdef __NEXT_RUNTIME__
#include <objc/objc-class.h>
#endif


static NSAutoreleasePool *objcl_autorelease_pool = NULL;


void
objcl_initialise_runtime (void)
{
  objcl_autorelease_pool = [[NSAutoreleasePool alloc] init];
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


OBJCL_OBJ_DATA
objcl_find_class (const char *class_name)
{
  Class class =
    NSClassFromString ([NSString stringWithUTF8String: class_name]);
  OBJCL_OBJ_DATA result = malloc (sizeof (struct objcl_object));
  const char *const typespec = @encode (Class);

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
  const char *const typespec = @encode (SEL);

  result->type = malloc (strlen (typespec) + 1);
  strcpy (result->type, typespec);
  result->data.sel_val = selector;

  return result;
}


const char *
objcl_class_name (OBJCL_OBJ_DATA class)
{
  const char *ns_name;
  char *name;
  Class cls = NULL;

  switch (class->type[0])
    {
    case '#':
      cls = class->data.class_val;
      break;
    case '@':
      cls = class->data.id_val;
      break;
    case 'E':
      cls = (id) class->data.exc_val;
      break;
    default:
      return NULL;
    }

  ns_name = [(NSStringFromClass (cls)) UTF8String];
  name = malloc (strlen (ns_name) + 1);
  strcpy (name, ns_name);

  return name;
}


const char *
objcl_selector_name (OBJCL_OBJ_DATA selector)
{
  const char *ns_name;
  char *name;

  if (strcmp (selector->type, @encode (SEL)) != 0)
    return NULL;

  ns_name = [(NSStringFromSelector (selector->data.sel_val))
              UTF8String];
  name = malloc (strlen (ns_name) + 1);
  strcpy (name, ns_name);

  return name;
}


IMP
objcl_get_method_implementation (OBJCL_OBJ_DATA object,
                                 OBJCL_OBJ_DATA selector)
{
  id obj;

  if (strcmp (selector->type, @encode (SEL)) != 0)
    return NULL;

  switch (object->type[0])
    {
    case '#':
      obj = object->data.class_val;
      break;
    case '@':
      obj = object->data.id_val;
      break;
    case 'E':
      obj = (id) object->data.exc_val;
      break;
    default:
      return NULL;
    }

#ifdef __NEXT_RUNTIME__
  return class_getInstanceMethod ([obj class],
                                  selector->data.sel_val)->method_imp;
#else
  return objc_msg_lookup (obj, selector->data.sel_val);
#endif
}
