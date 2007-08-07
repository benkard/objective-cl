(defpackage #:mulk.objective-cl.tests
  (:nicknames #:objcl-tests #:objective-cl-tests #:mulk.objcl-tests)
  (:use #:lift #:mulk.objective-cl #:cl)
  (:export #:run-all-tests))
(in-package #:mulk.objective-cl.tests)


(eval-when (:compile-toplevel)
  (objcl:install-reader-syntax))


(defun run-all-tests ()
  (objcl:initialise-runtime)
  (run-tests :suite 'objective-cl))


(deftestsuite objective-cl ()
  ())


(deftestsuite base-functions (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-same (find-objc-class 'ns-object)
                 (find-objc-class "NSObject")))
   ((ensure-null (find-objc-class 'nsobject)))
   ((ensure-same (find-objc-class 'ns-method-invocation)
                 (find-objc-class "NSMethodInvocation")))
   ((ensure-null (find-selector "mulkyStuff:withMagic:")))
   ((ensure-same (find-selector "self")
                 (find-selector '(self))))
   ((ensure-same (find-selector "stringWithCString:")
                 (find-selector '(:string-with-c-string))))
   ((ensure-same (find-selector "stringWithCString:encoding:")
                 (find-selector '(:string-with-c-string :encoding))))))


(deftestsuite method-invocation (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-error [NSObject 300]))
   ((ensure-error [300 self]))
   ((ensure-error ["abc" self]))
   ((ensure-error [NSObject selph]))
   ((ensure-same [NSObject self]
                 [NSObject class]))
   ((ensure-different [NSObject self]
                      [NSNumber self]))
   ((ensure-same [NSString stringWithCString: "Mulk."]
                 [NSString stringWithCString: "Mulk."]))
   ((ensure-different [NSString stringWithCString: "Mulk."]
                      [NSString stringWithCString: "Klum."]))
   ((ensure [NSString isSubclassOfClass: [NSObject class]]))
   ((ensure [NSString performSelector:
                        (find-selector "isSubclassOfClass:")
                      withObject: [NSObject class]]))))


(deftestsuite data-coercion (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-same [NSString stringWithCString: "Mulk."]
                 [NSString stringWithCString: "Mulk." encoding: 4]))
   ((ensure-same [NSString respondsToSelector: (find-selector "new")]
                 [NSString respondsToSelector: 'new]))
   ((ensure-same [NSString respondsToSelector: (find-selector "new")]
                 [NSString respondsToSelector: "new"]))
   ((ensure (typep [NSString isEqual: [NSString self]] 'boolean)))
   ((ensure (typep [NSString isEqual: [NSObject self]] 'boolean)))))


(deftestsuite exception-handling (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure (typep (handler-case
                       [NSString selph]
                     (exception (e) e))
                   'exception)))))


(deftestsuite reader-syntax (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-same [NSObject self]
                 (find-objc-class 'ns-object)))
   ((ensure-same [NSString stringWithCString: "Mulk."]
                 (invoke (find-objc-class 'ns-string)
                         :string-with-c-string "Mulk.")))
   ((ensure-same [NSString stringWithCString: "Mulk." encoding: 4]
                 (invoke (find-objc-class 'ns-string)
                         :string-with-c-string "Mulk." :encoding 4)))
   ((ensure-same [NSString performSelector:
                             (find-selector "isSubclassOfClass:")
                           withObject: [NSObject class]]
                 (invoke (find-objc-class 'ns-string)
                         :performSelector (find-selector "isSubclassOfClass")
                         :with-object (find-objc-class 'ns-object))))))
