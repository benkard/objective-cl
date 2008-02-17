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
  (:use #:stefil #:mulk.objective-cl #:cl)
  (:export #:run-all-tests #:objective-cl)
  (:shadowing-import-from #:objcl
                          #:struct #:union #:pointer #:oneway #:out #:in
                          #:inout #:const #:parse-typespec #:objective-c-class
                          #:bit-field #:opaque #:bycopy #:byref
                          #:primitive-invoke #:print-typespec-to-string
                          #:nominally #:find-objc-meta-class))
(in-package #:mulk.objective-cl.tests)
(in-root-suite)


(eval-when (:compile-toplevel)
  (objcl:install-reader-syntax))


(defun run-all-tests ()
  (objective-cl))


(defsuite objective-cl ()
  (objcl:initialise-runtime)
  (run-child-tests)
  (objcl:shutdown-runtime))
(in-suite objective-cl)


#.(prog1 nil
    (defparameter *readtable-backup* *readtable*)
    (setq *readtable* (copy-readtable))
    (setf (readtable-case *readtable*) :invert))
(deftest base-functions ()
  (is (objc-equal (find-objc-class 'ns-object)
                  (find-objc-class "NSObject")))
  (is (null (find-objc-class 'nsobject)))
  (is (objc-equal (find-objc-class 'ns-method-invocation)
                  (find-objc-class "NSMethodInvocation")))
  (is (typep (find-selector "mulkyStuff:withMagic:" nil)
             '(or null selector)))
  (is (objc-equal (find-selector "self")
                  (find-selector '(self))))
  (is (objc-equal (find-selector "stringWithUTF8String:")
                  (find-selector '(:string-with-u-t-f-8-string))))
  (is (objc-equal (find-selector "stringWithCString:encoding:")
                  (find-selector '(:string-with-c-string :encoding))))
  ;; Case-sensitivity.
  (is (objc-equal (find-selector "stringWithCString:encoding:")
                  (find-selector '(:stringWithCString :encoding))))
  (is (objc-equal (find-selector "stringWithUTF8String:")
                  (find-selector :stringWithUTF8String))))
#.(prog1 nil
    (setq *readtable* *readtable-backup*))


(deftest primitive-method-invocation ()
  (#+(or) (signals error (primitive-invoke (find-objc-class 'ns-object)
                                           'string 'id)))
  (signals error (primitive-invoke 300 'self 'id))
  (signals error (primitive-invoke "abc" 'self 'id))
  (#+(or) (signals error (primitive-invoke (find-objc-class 'ns-object)
                                           'selph 'id)))
  (is (objc-equal (primitive-invoke (find-objc-class 'ns-object)
                                    'self 'id)
                  (primitive-invoke (find-objc-class 'ns-object)
                                    'class 'objective-c-class)))
  (is (not (objc-equal (primitive-invoke (find-objc-class 'ns-object)
                                         'self 'id)
                       (primitive-invoke (find-objc-class 'ns-number)
                                         'self 'id))))
  (is (objc-equal (primitive-invoke (find-objc-class 'ns-string)
                                    :string-with-u-t-f-8-string 'id
                                    "Mulk.")
                  (primitive-invoke (find-objc-class 'ns-string)
                                    :string-with-u-t-f-8-string 'id
                                    "Mulk.")))
  (is (not (objc-equal (primitive-invoke (find-objc-class 'ns-string)
                                         :string-with-u-t-f-8-string 'id
                                         "Mulk.")
                       (primitive-invoke (find-objc-class 'ns-string)
                                         :string-with-u-t-f-8-string 'id
                                         "Klum."))))
  (is (primitive-invoke (find-objc-class 'ns-string)
                        :is-subclass-of-class
                        (first (parse-typespec "c" t))
                        (find-objc-class 'ns-object)))
  ;; performSelector:withObject: cannot be used with non-id return
  ;; types.
  #+(or)
  (is (primitive-invoke (find-objc-class 'ns-string)
                        '(:perform-selector :with-object) :char
                        (selector "isSubclassOfClass:")
                        (find-objc-class 'ns-object))))


(deftest method-invocation ()
  (signals error [NSObject 300])
  (signals error [300 self])
  (signals error ["abc" self])
  (signals error [NSObject selph])
  (is (objc-equal [NSObject self]
                  [NSObject class]))
  (is (not (objc-equal [NSObject self]
                       [NSNumber self])))
  (is (objc-equal [NSString stringWithUTF8String: "Mulk."]
                  [NSString stringWithUTF8String: "Mulk."]))
  (is (not (objc-equal [NSString stringWithUTF8String: "Mulk."]
                       [NSString stringWithUTF8String: "Klum."])))
  (is [NSString isSubclassOfClass: [NSObject class]])
  ;; performSelector:withObject: cannot be used with non-id return
  ;; types.
  #+(or)
  (is [NSString performSelector: (selector "stringWithUTF8String:")
                withObject: [NSObject class]]))


(deftest parsing-typespecs ()
  (is (equal (parse-typespec "@0:4{_NSRange=II}8")
             '(id ())))
  (is (equal (parse-typespec ":4{_NSRange=II}8")
             '(selector ())))
  (is (equal (parse-typespec "{_NSRange=II}8")
             '(struct () "_NSRange"
               (:unsigned-int ())
               (:unsigned-int ()))))
  (is (equal (parse-typespec "rnNoV^V[10rjd]4")
             ;; Actually, the order of the qualifiers doesn't
             ;; matter, which means that this test is dumber than
             ;; it ought to be.
             '(pointer (oneway out inout in const)
               (array (oneway)
                10
                (complex (const) (:double nil))))))
  (is (equal (parse-typespec "ROi")
             ;; Here, too, the order of the qualifiers is irrelevant.
             '(:int (bycopy byref))))
  (is (equal (parse-typespec "(?=)")
             '(union () "?")))
  (is (equal (parse-typespec "{?=rb123rjf456iii}")
             (if (eq objcl::+runtime-type+ :gnu)
                 '(struct () "?"
                   (bit-field (const) 123 456
                    (complex (const) (:float ())))
                   (:int ())
                   (:int ())
                   (:int ()))
                 '(struct () "?"
                   (bit-field (const) nil 123)
                   (complex (const) (:float ()))
                   (:unrecognised () #\4)
                   (:unrecognised () #\5)
                   (:unrecognised () #\6)
                   (:int ())
                   (:int ())
                   (:int ())))))
  (is (equal (parse-typespec "^[100{?=ii}]")
             '(pointer ()
               (array () 100
                (struct () "?" (:int ()) (:int ()))))))
  (is (equal (parse-typespec "{?=BiIlLqQfd@#:*?}")
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
  (let ((funky-spec (parse-typespec "{?=cC}")))
    (is (member funky-spec
                '((struct () "?"
                   (:char ())
                   (:unsigned-char ()))
                  (struct () "?"
                   (:int ((nominally :char)))
                   (:unsigned-int ((nominally :unsigned-char)))))
                :test #'equalp)))
  (let ((funky-spec (parse-typespec "{?=sS}")))
    (is (member funky-spec
                '((struct () "?"
                   (:short ())
                   (:unsigned-short ()))
                  (struct () "?"
                   (:int ((nominally :short)))
                   (:unsigned-int ((nominally :unsigned-short)))))
                :test #'equalp)))
  (is (equal (parse-typespec "{Mulk=*{Untermulk={Unteruntermulk=}}i}")
             '(struct () "Mulk"
               (:string ())
               (struct () "Untermulk"
                (struct () "Unteruntermulk"))
               (:int ()))))
  (is (equal (parse-typespec "^^{OpaqueStruct}")
             '(pointer ()
               (pointer ()
                (struct (opaque) "OpaqueStruct"))))))


(deftest printing-typespecs ()
  (is (equal (print-typespec-to-string '(id ()))
             "@"))
  (is (equal (print-typespec-to-string '(selector ()))
             ":"))
  (is (equal (print-typespec-to-string '(struct () "_NSRange"
                                         (:unsigned-int ())
                                         (:unsigned-int ())))
             "{_NSRange=II}"))
  (is (equal (print-typespec-to-string '(pointer (oneway out inout in const)
                                         (array (oneway)
                                          10
                                          (complex (const) (:double nil)))))
             ;; Actually, the order of the qualifiers doesn't
             ;; matter, which means that this test is dumber than
             ;; it ought to be.
             "VoNnr^V[10rjd]"))
  (is (equal (print-typespec-to-string '(:int (bycopy byref)))
             ;; Here, too, the order of the qualifiers is irrelevant.
             "ORi"))
  (is (equal (print-typespec-to-string '(union () "?"))
             "(?=)"))
  (is (equal (print-typespec-to-string
              (if (eq objcl::+runtime-type+ :gnu)
                  '(struct () "?"
                    (bit-field (const) 123 456
                     (complex (const) (:float ())))
                    (:int ())
                    (:int ())
                    (:int ()))
                  '(struct () "?"
                    (bit-field (const) nil 123)
                    (complex (const) (:float ()))
                    (:unrecognised () #\4)
                    (:unrecognised () #\5)
                    (:unrecognised () #\6)
                    (:int ())
                    (:int ())
                    (:int ()))))
             "{?=rb123rjf456iii}"))
  (is (equal (print-typespec-to-string '(pointer ()
                                         (array () 100
                                          (struct () "?" (:int ()) (:int ())))))
             "^[100{?=ii}]"))
  (is (equal (print-typespec-to-string '(struct () "?"
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
                                         (:unknown ())))
             "{?=BiIlLqQfd@#:*?}"))
  (is (equal (print-typespec-to-string '(struct () "?"
                                         (:char ())
                                         (:unsigned-char ())))
             "{?=cC}"))
  (is (equal (print-typespec-to-string '(struct () "?"
                                         (:short ())
                                         (:unsigned-short ())))
             "{?=sS}"))
  (is (equal (print-typespec-to-string
              '(struct () "Mulk"
                (:string ())
                (struct () "Untermulk"
                 (struct () "Unteruntermulk"))
                (:int ())))
             "{Mulk=*{Untermulk={Unteruntermulk=}}i}"))
  (is (equal (print-typespec-to-string
              '(pointer ()
                (pointer ()
                 (struct (opaque) "OpaqueStruct"))))
             "^^{OpaqueStruct}")))


(deftest data-coercion ()
  (is (objc-equal [NSString stringWithUTF8String: "Mulk."]
                  [NSString stringWithCString: "Mulk." encoding: 4]))
  (is (objc-equal [NSString respondsToSelector: (selector "new")]
                  [NSString respondsToSelector: 'new]))
  (is (objc-equal [NSString respondsToSelector: (selector "new")]
                  [NSString respondsToSelector: "new"]))
  (is (typep [NSString isEqual: [NSString self]] 'boolean))
  (is (typep [NSString isEqual: [NSObject self]] 'boolean)))


(deftest numbers ()
  (is (objc-equal [[NSDecimalNumber decimalNumberWithString:
                                      [NSString stringWithUTF8String: "-12345"]]
                   doubleValue]
                  -12345d0)))


(deftest exception-handling ()
  (is (typep (handler-case [NSString selph]
               (error (e) e))
             '(or no-such-selector message-not-understood)))
  (is (typep (handler-case [NSObject string]
               (error (e) e))
             'message-not-understood)))


(deftest reader-syntax ()
  (is (objc-equal [NSObject self]
                  (find-objc-class 'ns-object)))
  (is (objc-equal [NSString stringWithUTF8String: "Mulk."]
                  (invoke (find-objc-class 'ns-string)
                          :string-with-u-t-f-8-string "Mulk.")))
  (is (objc-equal [NSString stringWithCString: "Mulk." encoding: 4]
                  (invoke (find-objc-class 'ns-string)
                          :string-with-c-string "Mulk." :encoding 4)))
  ;; performSelector:withObject: cannot be used with non-id return
  ;; types.
  #+(or)
  (is (objc-equal [NSString performSelector: (selector "isSubclassOfClass:")
                            withObject: [NSObject self]]
                  (invoke (find-objc-class 'ns-string)
                          :perform-selector (selector "isSubclassOfClass:")
                          :with-object (invoke
                                        (find-objc-class 'ns-object)
                                        'self)))))


(defvar *class-counter* 0)


(deftest class-definition ()
  ;; FIXME: This test is broken.
  ;;
  ;; Reason:
  ;;
  ;;  OBJECTIVE-CL> (invoke-by-name
  ;;                 (invoke #<EXCEPTION NSGenericException {845E8E0}> 'reason)
  ;;                 "UTF8String")
  ;;  "subclass MLKTestString0(instance) should override length"
  ;;
  (let ((class-name (intern (format nil "~A~D"
                                    '#:mlk-test-string
                                    (incf *class-counter*))))
        (class nil)
        (instance nil))
    (setq class
          (is (c2mop:ensure-class class-name
                                  :direct-superclasses
                                  (list (find-objc-class "NSString"))
                                  :direct-slots '((:name foos
                                                   :type list
                                                   :initform nil
                                                   :initargs (:foos))
                                                  (:name foo-count
                                                   :foreign-type (:int ())))
                                  :metaclass (find-objc-meta-class "NSObject"))))
    (is (typep class 'objective-c-class))
    (setq instance (is (invoke class :string-with-u-t-f-8-string "Mulk.")))
    #+nil (is (typep instance class))
    (is (objc-equal instance
                    (invoke (find-objc-class 'ns-string)
                            :string-with-u-t-f-8-string "Mulk.")))
    (setf (slot-value instance 'foos) '(a b c))
    (setf (slot-value instance 'foo-count) 3)
    (is (equal (slot-value instance 'foos)
               (slot-value (invoke instance 'self) 'foos)))
    (is (equal (slot-value instance 'foo-count)
               (slot-value (invoke instance 'self) 'foo-count)))))
