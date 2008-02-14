/* -*- mode: objc; coding: utf-8 -*- */

#import <Foundation/NSObject.h>

void
objcl_initialise_instance_wrappers (void);

void
objcl_shutdown_instance_wrappers (void);

@interface NSObject (ObjectiveCLWrapperLink)
-(BOOL) __objcl_isBackedByLispInstance;
-(void) __objcl_setBackedByLispInstance: (BOOL)backed_p;
@end /* NSObject (ObjectiveCL) */
