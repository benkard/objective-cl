/* -*- mode: objc; coding: utf-8 -*- */

#import "Foundation/Foundation.h"
#include <objc/objc-api.h>

typedef struct objcl_object
{
  char* type;

  union
  {
    id id_val;
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


void
objcl_initialise_runtime (void);

void
objcl_shutdown_runtime (void);

void *
objcl_invoke_instance_method (OBJCL_OBJ_DATA receiver,
                              const char *method_name,
                              int argc,
                              ...);

void *
objcl_invoke_class_method (OBJCL_OBJ_DATA class,
                           const char *method_name,
                           int argc,
                           ...);

void *
objcl_find_class (const char *class_name);

/* Return a null-terminated list of type information strings.
   The first entry describes the type of the method's return value. */
char **
objcl_query_arglist_info (void *receiver,
                          const char *method_name);
