/* -*- mode: objc; coding: utf-8 -*- */

#import "NSObject-ObjectiveCLWrapperLink.h"
#import <Foundation/NSSet.h>
#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

/* A class is considered Lisp-backed if some of its methods are
   implemented as Lisp callbacks.  This is true if and only if
   @selector(retain) and @selector(release) are overridden by
   Objective-CL.  In this case, the corresponding Lisp objects are
   stored in a regular hash table instead of a weak one, as they may
   hold data (like CLOS slots) that we can't do without as long as the
   Objective-C instance is referenced from anywhere (where `anywhere'
   includes both the Lisp and Objective-C worlds). */
static NSMutableSet *lisp_backed_classes = nil;

void
objcl_initialise_instance_wrappers (void)
{
  if (lisp_backed_classes == nil)
    lisp_backed_classes = [[NSMutableSet alloc] init];
}

void
objcl_shutdown_instance_wrappers (void)
{
  if (lisp_backed_classes != nil)
    {
      [lisp_backed_classes release];
      lisp_backed_classes = nil;
    }
}

@implementation NSObject (ObjectiveCLWrapperLink)
+(BOOL) __objcl_isBackedByLispClass
{
  return [lisp_backed_classes containsObject: self];
}

+(void) __objcl_setBackedByLispClass: (BOOL)backed_p
{
  if (backed_p)
    [lisp_backed_classes addObject: self];
  else
    [lisp_backed_classes removeObject: self];
}
@end /* NSObject (ObjectiveCL) */
