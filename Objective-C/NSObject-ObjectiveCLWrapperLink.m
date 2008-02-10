/* -*- mode: objc; coding: utf-8 -*- */

#import "NSObject-ObjectiveCLWrapperLink.h"
#import <Foundation/NSDictionary.h>
#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

static NSMutableDictionary *instance_wrappers = NULL;

void
objcl_initialise_instance_wrappers (void)
{
  if (!instance_wrappers)
    instance_wrappers = [[NSMutableDictionary alloc] init];
}

void
objcl_shutdown_instance_wrappers (void)
{
  if (instance_wrappers)
    {
      [instance_wrappers release];
      instance_wrappers = NULL;
    }
}

@implementation NSObject (ObjectiveCLWrapperLink)
-(const char *) __objectiveCLWrapperID
{
  NSString *string = [instance_wrappers objectForKey: self];
  if (string != nil)
    return [string UTF8String];
  else
    return NULL;
}

-(void) __setObjectiveCLWrapperID: (const char *)wrapper_id
{
  [instance_wrappers setObject: [NSString stringWithUTF8String: wrapper_id]
                     forKey: self];
}

-(void) __removeObjectiveCLWrapperID
{
  [instance_wrappers removeObjectForKey: self];
}
@end /* NSObject (ObjectiveCL) */
