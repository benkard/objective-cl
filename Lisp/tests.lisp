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
                          #:nominally #:find-objc-meta-class
                          #:objcl-object-backed-by-lisp-class-p
                          #:foreign-class-registered-p
                          #:define-objective-c-method #:defobjcmethod
                          #:objective-c-generic-function #:objective-c-method
                          #:+nil+ #:+yes+ #:+no+ #:selector))
(in-package #:mulk.objective-cl.tests)
(in-root-suite)


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


#.(enable-objective-c-syntax)
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
#.(disable-objective-c-syntax)


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
                (struct (opaque) "OpaqueStruct")))))
  (is (equal (parse-typespec "^{_GSKeyBinding=ii@\"GSKeyBindingAction\"@\"GSKeyBindingTable\"}")
             '(pointer ()
               (struct () "_GSKeyBinding"
                (:int ())
                (:int ())
                (id ((:type "GSKeyBindingAction")))
                (id ((:type "GSKeyBindingTable")))))))
  (is (equal (parse-typespec "{?=\"next\"@\"GCObject\"\"previous\"@\"GCObject\"\"flags\"{?=}}")
             '(struct () "?"
               (id ((:type "GCObject") (:name "next")))
               (id ((:type "GCObject") (:name "previous")))
               (struct ((:name "flags")) "?")))))


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


#.(enable-objective-c-syntax)
(deftest data-coercion ()
  (is (objc-equal [NSString stringWithUTF8String: "Mulk."]
                  [NSString stringWithCString: "Mulk." encoding: 4]))
  (is (objc-equal [NSString respondsToSelector: (selector "new")]
                  [NSString respondsToSelector: 'new]))
  (is (objc-equal [NSString respondsToSelector: (selector "new")]
                  [NSString respondsToSelector: "new"]))
  (is (typep [NSString isEqual: [NSString self]] 'boolean))
  (is (typep [NSString isEqual: [NSObject self]] 'boolean)))
#.(disable-objective-c-syntax)


#.(enable-objective-c-syntax)
(deftest numbers ()
  (is (objc-equal [[NSDecimalNumber decimalNumberWithString:
                                      [NSString stringWithUTF8String: "-12345"]]
                   doubleValue]
                  -12345d0)))
#.(disable-objective-c-syntax)


#.(enable-objective-c-syntax)
(deftest exception-handling ()
  (is (typep (handler-case [NSString selph]
               (error (e) e))
             '(or no-such-selector message-not-understood)))
  (is (typep (handler-case [NSObject string]
               (error (e) e))
             'message-not-understood)))
#.(disable-objective-c-syntax)


#.(enable-objective-c-syntax)
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
#.(disable-objective-c-syntax)


#.(enable-method-syntax)
(deftest method-syntax ()
  (is (eq '#/stringWithCString:encoding:
          'objective-c-selectors:|stringWithCString:encoding:|))
  (is (fboundp '#/stringWithCString:encoding:))
  (is (typep (fdefinition '#/stringWithCString:encoding:) 'selector))
  (is (objc-equal (#/stringWithCString:encoding: (find-objc-class 'ns-string)
                                                 "Mulk."
                                                 4)
                  (invoke (find-objc-class 'ns-string)
                          :string-with-c-string "Mulk." :encoding 4)))
  (is (objc-equal (#/self (find-objc-class 'ns-array))
                  (find-objc-class 'ns-array)))
  (is (subtypep (#/class (#/new (find-objc-class 'ns-mutable-array)))
                (find-objc-class 'ns-mutable-array))))
#.(disable-method-syntax)


(deftest compiler-macros ()
  (flet ((compiler-macroexpand-1 (form &optional environment)
           (funcall (or (compiler-macro-function (car form))
                        #'(lambda (x y) (declare (ignore y)) x))
                    form
                    environment)))
    (is (equal (compiler-macroexpand-1 '(invoke x :is-equal y) nil)
               `(invoke-by-name x (selector '(:is-equal)) y)))
    (is (equal (compiler-macroexpand-1 '(invoke x 'self))
               `(invoke-by-name x (selector '(self)))))
    (is (equal (compiler-macroexpand-1 `(invoke x
                                                :string-with-c-string "Mulk."
                                                :encoding 4))
               `(invoke-by-name x
                                (selector '(:string-with-c-string :encoding))
                                "Mulk."
                                4)))
    (is (equal (compiler-macroexpand-1 `(invoke-by-name (+ 1 2) 'self))
               `(invoke-by-name (+ 1 2) (selector 'self))))
    (is (equal (compiler-macroexpand-1 `(invoke-by-name (+ 1 2) '(:foo :bar)
                                                        x y))
               `(invoke-by-name (+ 1 2) (selector '(:foo :bar)) x y)))
    (is (equal (compiler-macroexpand-1 `(primitive-invoke (+ 1 2) '(:foo :bar)
                                                          x y))
               `(primitive-invoke (+ 1 2) (selector '(:foo :bar)) x y)))
    (is (equal (car (compiler-macroexpand-1 `(selector '(:foo :bar))))
               'load-time-value))
    (is (not (equal (car (compiler-macroexpand-1 `(selector (:foo :bar))))
                    'load-time-value)))
    (is (not (equal (car (compiler-macroexpand-1 `(selector (car (:foo :bar)))))
                    'load-time-value)))
    (is (not (equal (car (compiler-macroexpand-1 `(selector `(,x ,y))))
                    'load-time-value)))))


(defvar *class-counter* 0)


(deftest class-definition ()
  (let ((class-name (intern (format nil "~A~D"
                                    '#:mlk-test-string
                                    (incf *class-counter*))))
        (numclass-name (intern (format nil "~A~D"
                                       '#:mlk-test-number
                                       (incf *class-counter*))
                               '#:objective-c-classes))
        (subnumclass-name (intern (format nil "~A~D"
                                          '#:mlk-test-sub-number
                                          (incf *class-counter*))
                                  '#:objective-c-classes))
        (class nil)
        (instance nil)
        (numinstance nil)
        (subnuminstance nil))
    (is (find-objc-class "NSNumber"))
    (setq class
          (is (c2mop:ensure-class class-name
                                  :direct-superclasses
                                  (list (find-objc-class "NSObject"))
                                  :direct-slots '((:name foos
                                                   :type list
                                                   :initargs (:foos))
                                                  (:name foo-count
                                                   :foreign-type :int))
                                  :metaclass (find-objc-meta-class "NSObject"))))

    ;; Class initialisation.
    (is (not (foreign-class-registered-p class)))

    ;; Method definition.
    #.(enable-method-syntax)
    (is (eval `(define-objective-c-generic-function #/prettifyNumber:
                   (self number))))

    (is (eval `(define-objective-c-class ,numclass-name (ns::ns-object) ())))
    (is (eval `(define-objective-c-method #/prettifyNumber: :int
                   ((self ,numclass-name) (number ns::ns-number))
                 (- (#/intValue number)))))

    (is (eval `(define-objective-c-class ,subnumclass-name (,numclass-name) ())))
    (is (eval `(define-objective-c-method #/prettifyNumber: :int
                   ((self ,subnumclass-name) (number ns::ns-number))
                 (* 2 (super)))))

    ;; We may omit DEFINE-OBJECTIVE-C-GENERIC-FUNCTION.
    (is (eval `(define-objective-c-method #/foo:bar:stuff:do: :int
                   ((x ,class-name)
                    (y :int)
                    z
                    (a t #+(or) (eql +nil+))
                    (b ns::ns-number)
                    &rest rest)
                 (declare (ignore z rest))
                 (+ y 20))))
    #.(disable-method-syntax)

    ;; Sanity checks.
    (is (typep class 'objective-c-class))
    (setq instance (is (invoke (invoke class 'alloc) 'init)))
    (setq numinstance (is (invoke (find-objc-class numclass-name) 'new)))
    (setq subnuminstance (is (invoke (find-objc-class subnumclass-name) 'new)))

    ;; Class finalisation.  (Should be automatic upon instance
    ;; creation.)
    (is (foreign-class-registered-p class))

    ;; Method calls.
    (is (= 170 (invoke instance :foo 150 :bar nil :stuff nil :do 100)))
    (is (= -30 (invoke numinstance :prettify-number 30)))
    (is (= -60 (invoke subnuminstance :prettify-number 30)))

    ;; Object identity preservation.
    (is (eql instance
             (invoke instance 'self)))

    ;; FIXME: What's wrong with the following line?  It makes CMUCL
    ;; throw weird errors.
    #+(or) (is (typep instance class-name))

    ;; Slot initialisation.
    (is (not (slot-boundp instance 'foos)))
    (is (slot-boundp instance 'foo-count)) ;foreign slots are always SLOT-BOUNDP

    ;; Slot handling.
    (setf (slot-value instance 'foos) '(a b c))
    (setf (slot-value instance 'foo-count) 3)

    (is (slot-boundp instance 'foos))

    ;; Native slots.
    (is (objcl-object-backed-by-lisp-class-p instance))

    (is (equal (slot-value instance 'foos)
               (slot-value (invoke instance 'self) 'foos)))
    (is (typep (slot-value (invoke instance 'self) 'foos)
               'list))
    (is (equal (slot-value (invoke instance 'self) 'foos)
               '(a b c)))

    ;; Foreign slots.
    (is (equal (slot-value instance 'foo-count)
               (slot-value (invoke instance 'self) 'foo-count)))
    (is (typep (slot-value (invoke instance 'self) 'foo-count)
               'integer))
    (is (equal (slot-value (invoke instance 'self) 'foo-count)
               3))))
