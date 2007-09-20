/* -*- mode: objc; coding: utf-8 -*- */

#import "Foundation/Foundation.h"
#include <objc/objc-api.h>

#ifdef USE_LIBFFI
#include <ffi.h>
#endif


extern NSException *objcl_oom_exception;


typedef struct objcl_object
{
  char* type;

  union
  {
    id id_val;
    Class class_val;
    NSException *exc_val;
    SEL sel_val;
    char char_val;
    short short_val;
    int int_val;
    long long_val;
    long long long_long_val;
    float float_val;
    double double_val;
    BOOL bool_val;
    char *charptr_val;
    void *ptr_val;
  } data;
} *OBJCL_OBJ_DATA;


#define EXCEPTION_TYPESPEC "ERROR"


void
objcl_initialise_runtime (void);

void
objcl_shutdown_runtime (void);

OBJCL_OBJ_DATA
objcl_invoke_method (OBJCL_OBJ_DATA receiver,
                     SEL method_selector,
                     int argc,
                     ...);

id
objcl_invoke_with_types (int argc,
                         char *return_typespec,
                         char *arg_typespecs[],
                         void *return_value,
                         void **argv);

Class
objcl_find_class (const char *class_name);

SEL
objcl_find_selector (const char *selector_name);

/* Return a null-terminated list of type information strings.
   The first entry describes the type of the method's return value. */
char **
objcl_query_arglist_info (void *receiver,
                          const char *method_name);


const char *
objcl_class_name (Class class);

const char *
objcl_selector_name (SEL selector);

IMP
objcl_get_method_implementation (id object,
                                 SEL selector);

BOOL
objcl_object_is_class (id obj);

BOOL
objcl_object_is_meta_class (id obj);

Class
objcl_object_get_class (id obj);

Class
objcl_object_get_meta_class (id obj);

id
objcl_get_nil (void);

/* In principle, we do not know whether a BOOL fits into a long.  In
   practise, it is very likely. */
long
objcl_get_yes ();

long
objcl_get_no ();
