/* -*- mode: objc; coding: utf-8 -*- */

#import <Foundation/NSObject.h>

void
objcl_initialise_instance_wrappers (void);

void
objcl_shutdown_instance_wrappers (void);

@interface NSObject (ObjectiveCLWrapperLink)
-(const char *) __objectiveCLWrapperID;
-(void) __setObjectiveCLWrapperID: (const char *)wrapper_id;
-(void) __removeObjectiveCLWrapperID;

/* Classes can't be wrapped at the moment. */
/*
+(const char *) __objectiveCLWrapperID;
+(const char *) __setObjectiveCLWrapperID: (const char *)wrapper_id;
+(const char *) __removeObjectiveCLWrapperID;
*/
@end /* NSObject (ObjectiveCL) */
