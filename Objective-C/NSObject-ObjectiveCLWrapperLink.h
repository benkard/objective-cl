/* -*- mode: objc; coding: utf-8 -*- */

#import <Foundation/NSObject.h>

void
objcl_initialise_instance_wrappers (void);

void
objcl_shutdown_instance_wrappers (void);

@interface NSObject (ObjectiveCLWrapperLink)
+(BOOL) __objcl_isBackedByLispClass;
+(void) __objcl_setBackedByLispClass: (BOOL)backed_p;
@end /* NSObject (ObjectiveCL) */
