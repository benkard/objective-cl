/* Copyright 2007, Matthias Andreas Benkard. */

#ifndef __pyobjc_H
#define __pyobjc_H

#include <stdlib.h>
#include <stdio.h>
#include "libobjcl.h"

#ifdef __NEXT_RUNTIME__
#include "pyobjc-compat.h"
#endif

#ifndef __OBJC2__
#define NO_OBJC2_RUNTIME
#endif

/* #define MAC_OS_X_VERSION_MIN_REQUIRED MAC_OS_X_VERSION_10_4 */

#ifdef __NEXT_RUNTIME__

#ifndef APPLE_RUNTIME
#define APPLE_RUNTIME
#endif

#import <objc/objc-class.h>

#else /* !__NEXT_RUNTIME__ */

#ifndef GNU_RUNTIME
#define GNU_RUNTIME
#endif

#import <objc/objc-api.h>
#ifndef __CXX__
#define bool BOOL
#endif

#endif /* __NEXT_RUNTIME__ */


#define PyMem_Free free
#define PyMem_Malloc malloc
#define Py_ssize_t ssize_t

#ifdef OOM_KILL
#define PyErr_NoMemory() [objcl_oom_exception raise]
#else
#define PyErr_NoMemory() NSLog (@"ERROR: Memory exhausted.");
#endif

#include "objc_support.h"
#include "libffi_support.h"

#endif /* __pyobjc_H */
