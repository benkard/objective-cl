/* Copyright (c) 1996,97,98 by Lele Gaifax.  All Rights Reserved
 * Copyright (2) 2003 Ronald Oussoren
 *
 * This software may be used and distributed freely for any purpose
 * provided that this notice is included unchanged on any and all
 * copies. The author does not warrant or guarantee this software in
 * any way.
 *
 * This file is part of the PyObjC package.
 *
 * RCSfile: objc_support.h,v
 * Revision: 1.16
 * Date: 1998/08/18 15:35:57
 *
 * Created Tue Sep 10 14:11:38 1996.
 *
 * TODO: the functions exported by this file should be changed, the names
 * should start with 'PyObjC' and should be the same as the names used in
 * pyobjc-api.h (where appropriate).
 */

#ifndef _objc_support_H
#define _objc_support_H

#ifdef GNU_RUNTIME

#  include "objc-runtime-gnu.h"

#else /* NeXTSTEP / Mac OS X */

#  include "objc-runtime-apple.h"

#endif 

extern ssize_t PyObjCRT_SizeOfReturnType(const char* type);
extern ssize_t PyObjCRT_SizeOfType(const char *type);
extern ssize_t PyObjCRT_AlignOfType(const char *type);
extern const char *PyObjCRT_SkipTypeSpec (const char *type);
extern const char* PyObjCRT_SkipTypeQualifiers (const char* type);

extern int PyObjCRT_SetupClass(
	Class, Class, const char*, Class, Class, ssize_t, struct objc_ivar_list*,
	struct objc_protocol_list*);
extern void PyObjCRT_ClearClass(Class cls);

#endif /* _objc_support_H */
