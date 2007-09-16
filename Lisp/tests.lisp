(defpackage #:mulk.objective-cl.tests
  (:nicknames #:objcl-tests #:objective-cl-tests #:mulk.objcl-tests)
  (:use #:lift #:mulk.objective-cl #:cl)
  (:export #:run-all-tests)
  (:shadowing-import-from #:objcl
                          #:struct #:union #:pointer #:oneway #:out #:in
                          #:inout #:const #:parse-typespec #:objc-class
                          #:bit-field #:opaque #:bycopy #:byref
                          #:primitive-invoke))
(in-package #:mulk.objective-cl.tests)


(eval-when (:compile-toplevel)
  (objcl:install-reader-syntax))


(defun run-all-tests ()
  (objcl:initialise-runtime)
  (run-tests :suite 'objective-cl))


(deftestsuite objective-cl ()
  ())


(defrandom-instance double objective-cl
  (if (zerop (random 2))
      (random most-positive-double-float)
      (- (random (abs most-negative-double-float)))))


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


(deftestsuite primitive-method-invocation (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-error (primitive-invoke (find-objc-class 'ns-object)
                                    'string 'id)))
   ((ensure-error (primitive-invoke 300 'self 'id)))
   ((ensure-error (primitive-invoke "abc" 'self 'id)))
   ((ensure-error (primitive-invoke (find-objc-class 'ns-object)
                                    'selph 'id)))
   ((ensure-same (primitive-invoke (find-objc-class 'ns-object)
                                   'self 'id)
                 (primitive-invoke (find-objc-class 'ns-object)
                                   'class 'objc-class)))
   ((ensure-different (primitive-invoke (find-objc-class 'ns-object)
                                   'self 'id)
                      (primitive-invoke (find-objc-class 'ns-number)
                                   'self 'id)))
   ((ensure-same (primitive-invoke (find-objc-class 'ns-string)
                                   :string-with-c-string 'id
                                   "Mulk.")
                 (primitive-invoke (find-objc-class 'ns-string)
                                   :string-with-c-string 'id
                                   "Mulk.")))
   ((ensure-different (primitive-invoke (find-objc-class 'ns-string)
                                        :string-with-c-string 'id
                                        "Mulk.")
                      (primitive-invoke (find-objc-class 'ns-string)
                                        :string-with-c-string 'id
                                        "Klum.")))
   ((ensure (primitive-invoke (find-objc-class 'ns-string)
                              :is-subclass-of-class :boolean
                              (find-objc-class 'ns-object))))
   ((ensure (primitive-invoke (find-objc-class 'ns-string)
                              '(:perform-selector :with-object) :boolean
                              (selector "isSubclassOfClass:")
                              (find-objc-class 'ns-object))))))


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
                        (selector "isSubclassOfClass:")
                      withObject: [NSObject class]]))))


(deftestsuite parsing-typespecs (objective-cl)
  ()
  (:equality-test #'equal)
  (:tests
   ((ensure-same (parse-typespec "@0:4{_NSRange=II}8")
                 '(id ())))
   ((ensure-same (parse-typespec ":4{_NSRange=II}8")
                 '(selector ())))
   ((ensure-same (parse-typespec "{_NSRange=II}8")
                 '(struct () "_NSRange"
                   (:unsigned-int ())
                   (:unsigned-int ()))))
   ((ensure-same (parse-typespec "rnNoV^V[10rjd]4")
                 ;; Actually, the order of the qualifiers doesn't
                 ;; matter, which means that this test is dumber than
                 ;; it ought to be.
                 '(pointer (oneway out inout in const)
                   (array (oneway)
                    10
                    (complex (const) (:double nil))))))
   ((ensure-same (parse-typespec "ROi")
                 ;; Here, too, the order of the qualifiers is irrelevant.
                 '(:int (bycopy byref))))
   ((ensure-same (parse-typespec "(?=)")
                 '(union () "?")))
   ((ensure-same (parse-typespec "{?=rb123rjf456iii}")
                 '(struct () "?"
                   (bit-field (const) 123 456
                    (complex (const) (:float ())))
                   (:int ())
                   (:int ())
                   (:int ()))))
   ((ensure-same (parse-typespec "^[100{?=ii}]")
                 '(pointer ()
                   (array () 100
                    (struct () "?" (:int ()) (:int ()))))))
   ((ensure-same (parse-typespec "{?=BcCsSiIlLqQfd@#:*?}")
                 '(struct () "?"
                   (:boolean ())
                   (:char ())
                   (:unsigned-char ())
                   (:short ())
                   (:unsigned-short ())
                   (:int ())
                   (:unsigned-int ())
                   (:long ())
                   (:unsigned-long ())
                   (:long-long ())
                   (:unsigned-long-long ())
                   (:float ())
                   (:double ())
                   (id ()) (objc-class ()) (selector ())
                   (:string ())
                   (:unknown ()))))
   ((ensure-same (parse-typespec "{Mulk=*{Untermulk={Unteruntermulk=}}i}")
                 '(struct () "Mulk"
                   (:string ())
                   (struct () "Untermulk"
                    (struct () "Unteruntermulk"))
                   (:int ()))))
   ((ensure-same (parse-typespec "^^{OpaqueStruct}")
                 '(pointer ()
                   (pointer ()
                    (struct (opaque) "OpaqueStruct")))))))


(deftestsuite data-coercion (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-same [NSString stringWithCString: "Mulk."]
                 [NSString stringWithCString: "Mulk." encoding: 4]))
   ((ensure-same [NSString respondsToSelector: (selector "new")]
                 [NSString respondsToSelector: 'new]))
   ((ensure-same [NSString respondsToSelector: (selector "new")]
                 [NSString respondsToSelector: "new"]))
   ((ensure (typep [NSString isEqual: [NSString self]] 'boolean)))
   ((ensure (typep [NSString isEqual: [NSObject self]] 'boolean)))))


(deftestsuite numbers (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-same [[NSDecimalNumber
                   decimalNumberWithString:
                     [NSString stringWithCString:
                        "-12345"]]
                  doubleValue]
                 -12345d0))))


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
                             (selector "isSubclassOfClass:")
                           withObject: [NSObject self]]
                 (invoke (find-objc-class 'ns-string)
                         :perform-selector (selector "isSubclassOfClass:")
                         :with-object (invoke
                                       (find-objc-class 'ns-object)
                                       'self))))))
