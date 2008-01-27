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

#import "Foundation/Foundation.h"
#include <objc/objc-api.h>

#include "../config.h"

#ifdef USE_LIBFFI
#ifdef HAVE_FFI_H
#include <ffi.h>
#elif HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#else
/* We are using our own build of libffi. */
#include <ffi.h>
#endif
#endif

extern NSException *objcl_oom_exception;


void
objcl_initialise_runtime (void);

void
objcl_shutdown_runtime (void);

id
objcl_invoke_with_types (int argc,
                         char *return_typespec,
                         char *arg_typespecs[],
                         void *return_value,
                         void **argv);

Class
objcl_find_class (const char *class_name);

Class
objcl_find_meta_class (const char *class_name);

SEL
objcl_find_selector (const char *selector_name);

SEL
objcl_intern_selector (const char *selector_name);

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
objcl_get_yes (void);

long
objcl_get_no (void);

const char *
objcl_get_runtime_type (void);

long
objcl_sizeof_type (const char *typespec);

long
objcl_sizeof_return_type (const char *typespec);

long
objcl_alignof_type (const char *typespec);
