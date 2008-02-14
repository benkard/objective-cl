/* -*- mode: objc; coding: utf-8 -*- */

#import "NSObject-ObjectiveCLWrapperLink.h"
#import <Foundation/NSSet.h>
#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

static NSMutableSet *lisp_backed_objects = nil;

void
objcl_initialise_instance_wrappers (void)
{
  if (lisp_backed_objects == nil)
    lisp_backed_objects = [[NSMutableSet alloc] init];
}

void
objcl_shutdown_instance_wrappers (void)
{
  if (lisp_backed_objects != nil)
    {
      [lisp_backed_objects release];
      lisp_backed_objects = nil;
    }
}

@implementation NSObject (ObjectiveCLWrapperLink)
-(BOOL) __objcl_isBackedByLispInstance
{
  return [lisp_backed_objects containsObject: self];
}

-(void) __objcl_setBackedByLispInstance: (BOOL)backed_p
{
  if (backed_p)
    [lisp_backed_objects addObject: self];
  else
    [lisp_backed_objects removeObject: self];
}
@end /* NSObject (ObjectiveCL) */
