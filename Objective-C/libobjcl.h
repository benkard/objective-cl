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

#import "Foundation/NSException.h"
#import "Foundation/NSLock.h"

#include <objc/objc-api.h>

#include "../config.h"

#import "PyObjC/pyobjc.h"
#import "PyObjC/objc_support.h"
#import "PyObjC/objc-runtime-compat.h"

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

#ifdef __NEXT_RUNTIME__
typedef Ivar IVAR_T;
#else
typedef struct objc_ivar *IVAR_T;
#endif


extern NSException *objcl_oom_exception;
extern id objcl_current_exception;
extern NSRecursiveLock *objcl_current_exception_lock;


void
objcl_initialise_runtime (void);

void
objcl_shutdown_runtime (void);

id
objcl_invoke_with_types (int argc,
                         Class superclass_for_send_super,
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

Class
objcl_class_superclass (Class class);

const char *
objcl_selector_name (SEL selector);

IMP
objcl_get_method_implementation (id object,
                                 SEL selector,
                                 Class superclass_for_send_super);

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

int
objcl_objc2_p (void);

long
objcl_sizeof_type (const char *typespec);

long
objcl_sizeof_return_type (const char *typespec);

long
objcl_alignof_type (const char *typespec);

void
objcl_set_slot_value (id obj, const char *ivar_name, void *value);

void *
objcl_slot_value (id obj, const char *ivar_name);

/* The following function returns a freshly consed array that the caller
   must deallocate. */
IVAR_T *
objcl_class_direct_slots (Class class,
                          unsigned int *count,
                          unsigned int *element_size);

const char *
objcl_slot_name (IVAR_T ivar);

const char *
objcl_slot_type (IVAR_T ivar);

IMP
objcl_create_imp (IMP callback,
                  int argc,
                  const char *return_typespec,
                  const char *arg_typespecs[]);

void
objcl_acquire_lock (id lock);

void
objcl_release_lock (id lock);

Class
objcl_create_class (const char *class_name,
                    Class superclass,
                    int protocol_number,
                    const char *protocol_names[],
                    int ivar_number,
                    const char *ivar_names[],
                    const char *ivar_typespecs[]);

void
objcl_add_method (Class class,
                  SEL method_name,
                  IMP callback,
                  int argc,
                  const char *return_typespec,
                  const char *arg_typespecs[],
                  const char *signature);

void
objcl_finalise_class (Class class);
