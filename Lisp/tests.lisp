;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007  Matthias Andreas Benkard.
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public License
;;;; as published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(defpackage #:mulk.objective-cl.tests
  (:nicknames #:objcl-tests #:objective-cl-tests #:mulk.objcl-tests)
  (:use #:lift #:mulk.objective-cl #:cl)
  (:export #:run-all-tests)
  (:shadowing-import-from #:objcl
                          #:struct #:union #:pointer #:oneway #:out #:in
                          #:inout #:const #:parse-typespec #:objective-c-class
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


#.(prog1 nil
    (defparameter *readtable-backup* *readtable*)
    (setq *readtable* (copy-readtable))
    (setf (readtable-case *readtable*) :invert))
(deftestsuite base-functions (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-same (find-objc-class 'ns-object)
                 (find-objc-class "NSObject")))
   ((ensure-null (find-objc-class 'nsobject)))
   ((ensure-same (find-objc-class 'ns-method-invocation)
                 (find-objc-class "NSMethodInvocation")))
   ((ensure (typep (find-selector "mulkyStuff:withMagic:" nil)
                   '(or null selector))))
   ((ensure-same (find-selector "self")
                 (find-selector '(self))))
   ((ensure-same (find-selector "stringWithUTF8String:")
                 (find-selector '(:string-with-u-t-f-8-string))))
   ((ensure-same (find-selector "stringWithCString:encoding:")
                 (find-selector '(:string-with-c-string :encoding))))
   ;; Case-sensitivity.
   ((ensure-same (find-selector "stringWithCString:encoding:")
                 (find-selector '(:stringWithCString :encoding))))
   ((ensure-same (find-selector "stringWithUTF8String:")
                 (find-selector :stringWithUTF8String)))))
#.(prog1 nil
    (setq *readtable* *readtable-backup*))


(deftestsuite primitive-method-invocation (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   (#+(or) (ensure-error (primitive-invoke (find-objc-class 'ns-object)
                                    'string 'id)))
   ((ensure-error (primitive-invoke 300 'self 'id)))
   ((ensure-error (primitive-invoke "abc" 'self 'id)))
   (#+(or) (ensure-error (primitive-invoke (find-objc-class 'ns-object)
                                    'selph 'id)))
   ((ensure-same (primitive-invoke (find-objc-class 'ns-object)
                                   'self 'id)
                 (primitive-invoke (find-objc-class 'ns-object)
                                   'class 'objective-c-class)))
   ((ensure-different (primitive-invoke (find-objc-class 'ns-object)
                                   'self 'id)
                      (primitive-invoke (find-objc-class 'ns-number)
                                   'self 'id)))
   ((ensure-same (primitive-invoke (find-objc-class 'ns-string)
                                   :string-with-u-t-f-8-string 'id
                                   "Mulk.")
                 (primitive-invoke (find-objc-class 'ns-string)
                                   :string-with-u-t-f-8-string 'id
                                   "Mulk.")))
   ((ensure-different (primitive-invoke (find-objc-class 'ns-string)
                                        :string-with-u-t-f-8-string 'id
                                        "Mulk.")
                      (primitive-invoke (find-objc-class 'ns-string)
                                        :string-with-u-t-f-8-string 'id
                                        "Klum.")))
   ((ensure-same +yes+
                 (primitive-invoke (find-objc-class 'ns-string)
                                   :is-subclass-of-class
                                   (first (parse-typespec "c" t))
                                   (find-objc-class 'ns-object))))
   ;; performSelector:withObject: cannot be used with non-id return
   ;; types.
   #+(or)
   ((ensure-same +yes+
                 (primitive-invoke (find-objc-class 'ns-string)
                                   '(:perform-selector :with-object) :char
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
   ((ensure-same [NSString stringWithUTF8String: "Mulk."]
                 [NSString stringWithUTF8String: "Mulk."]))
   ((ensure-different [NSString stringWithUTF8String: "Mulk."]
                      [NSString stringWithUTF8String: "Klum."]))
   ((ensure [NSString isSubclassOfClass: [NSObject class]]))
   ;; performSelector:withObject: cannot be used with non-id return
   ;; types.
   #+(or)
   ((ensure [NSString performSelector:
                        (selector "stringWithUTF8String:")
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
                 (if (eq objcl::+runtime-type+ :gnu)
                     '(struct () "?"
                       (bit-field (const) 123 456
                        (complex (const) (:float ())))
                       (:int ())
                       (:int ())
                       (:int ()))
                     '(struct () "?"
                       (bit-field (const) nil 123 nil)
                       (complex (const) (:float ()))
                       (:unrecognised ((:type-specifier #\4)))
                       (:unrecognised ((:type-specifier #\5)))
                       (:unrecognised ((:type-specifier #\6)))
                       (:int ())
                       (:int ())
                       (:int ())))))
   ((ensure-same (parse-typespec "^[100{?=ii}]")
                 '(pointer ()
                   (array () 100
                    (struct () "?" (:int ()) (:int ()))))))
   ((ensure-same (parse-typespec "{?=BiIlLqQfd@#:*?}")
                 '(struct () "?"
                   (:boolean ())
                   (:int ())
                   (:unsigned-int ())
                   (:long ())
                   (:unsigned-long ())
                   (:long-long ())
                   (:unsigned-long-long ())
                   (:float ())
                   (:double ())
                   (id ()) (objective-c-class ()) (selector ())
                   (:string ())
                   (:unknown ()))))
   ((ensure (let ((funky-spec (parse-typespec "{?=cC}")))
              (member funky-spec
                      '((struct () "?"
                         (:char ())
                         (:unsigned-char ()))
                        (struct () "?"
                         (:int ())
                         (:unsigned-int ())))
                      :test #'equalp))))
   ((ensure (let ((funky-spec (parse-typespec "{?=sS}")))
              (member funky-spec
                      '((struct () "?"
                         (:short ())
                         (:unsigned-short ()))
                        (struct () "?"
                         (:int ())
                         (:unsigned-int ())))
                      :test #'equalp))))
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
   ((ensure-same [NSString stringWithUTF8String: "Mulk."]
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
                     [NSString stringWithUTF8String:
                        "-12345"]]
                  doubleValue]
                 -12345d0))))


(deftestsuite exception-handling (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure (typep (handler-case
                       [NSString selph]
                     (error (e) e))
                   '(or no-such-selector message-not-understood))))
   ((ensure (typep (handler-case
                       [NSObject string]
                     (error (e) e))
                   'message-not-understood)))))


(deftestsuite reader-syntax (objective-cl)
  ()
  (:equality-test #'objc-equal)
  (:tests
   ((ensure-same [NSObject self]
                 (find-objc-class 'ns-object)))
   ((ensure-same [NSString stringWithUTF8String: "Mulk."]
                 (invoke (find-objc-class 'ns-string)
                         :string-with-u-t-f-8-string "Mulk.")))
   ((ensure-same [NSString stringWithCString: "Mulk." encoding: 4]
                 (invoke (find-objc-class 'ns-string)
                         :string-with-c-string "Mulk." :encoding 4)))
   ;; performSelector:withObject: cannot be used with non-id return
   ;; types.
   #+(or)
   ((ensure-same [NSString performSelector:
                             (selector "isSubclassOfClass:")
                           withObject: [NSObject self]]
                 (invoke (find-objc-class 'ns-string)
                         :perform-selector (selector "isSubclassOfClass:")
                         :with-object (invoke
                                       (find-objc-class 'ns-object)
                                       'self))))))
