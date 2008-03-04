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

(in-package #:mulk.objective-cl)


(defgeneric objc-eql (x y)
  (:documentation "Test whether two references point to the same object.

## Arguments and Values:

*x* --- an **object**.

*y* --- an **object**.

Returns: *the-same-p* --- a **generalized boolean**.


## Description:

If at least one of the **object**s *x* and *y* is not an Objective-C
instance wrapper, __objc-eql__ behaves the same as __eql__.

For Objective-C instance wrappers, __objc-eql__ compares the pointers
represented by these instances.  If the pointers are _pointer-eq_ (see
the [CFFI manual][] for details), __objc-eql__ returns **true**.
Otherwise, it returns **false**.


## Examples:

    (objc-eql (find-objc-class 'ns-string)
              (find-objc-class 'ns-string))
     ;=> T

    (objc-eql (find-objc-class 'ns-string)
              (find-objc-class 'ns-object))
     ;=> NIL

    (setq greeting1 (invoke (find-objc-class 'ns-string)
                            :string-with-u-t-f-8-string \"Mulk.\"))
     ;=> #<GSCBufferString `Mulk.' {8109A38}>

    (setq greeting2 (invoke (find-objc-class 'ns-string)
                            :string-with-u-t-f-8-string \"Mulk.\"))
     ;=> #<GSCBufferString `Mulk.' {8109DB0}>

    (objc-equal greeting1 greeting2)
     ;=> T

    (objc-eql greeting1 greeting2)
     ;=> NIL

    (objc-equal greeting1 greeting1)
     ;=> T

    (objc-eql greeting1 greeting1)
     ;=> T


## See Also:

  __objc-equal__

[CFFI manual]: http://common-lisp.net/project/cffi/manual/html_node/index.html"))


(defgeneric objc-equal (x y)
  (:documentation "Test whether two objects are equal.

## Arguments and Values:

*x* --- an **object**.

*y* --- an **object**.

Returns: *equal-p* --- a **generalized boolean**.


## Description:

If at least one of the **object**s *x* and *y* is not an Objective-C
instance wrapper, __objc-equal__ behaves the same as __equal__.

For Objective-C instance wrappers, __objc-equal__ behaves the same as
`(or (objc-eql x y) (invoke x :is-equal y) (invoke y :is-equal x))`,
except that it converts the return values of the invocations into
**generalized boolean**s before evaluating said expression.  (Please
note that, as there is no way to distinguish methods that return
booleans from those that return numbers in the Objective-C runtime, the
invocations will return numbers.)


## Examples:

    (objc-equal (find-objc-class 'ns-string)
                (find-objc-class 'ns-string))
     ;=> T

    (objc-equal (find-objc-class 'ns-string)
                (find-objc-class 'ns-object))
     ;=> NIL

    (setq greeting1 (invoke (find-objc-class 'ns-string)
                            :string-with-u-t-f-8-string \"Mulk.\"))
     ;=> #<GSCBufferString `Mulk.' {8109A38}>

    (setq greeting2 (invoke (find-objc-class 'ns-string)
                            :string-with-u-t-f-8-string \"Mulk.\"))
     ;=> #<GSCBufferString `Mulk.' {8109DB0}>

    (objc-equal greeting1 greeting2)
     ;=> T

    (objc-eql greeting1 greeting2)
     ;=> NIL

    (objc-equal greeting1 greeting1)
     ;=> T

    (objc-eql greeting1 greeting1)
     ;=> T


## See Also:

  __objc-eql__, __invoke__"))


(defun truep (b)
  (or (eq b t)
      (and (numberp b)
           (not (eql b +no+)))))


(defun id-eql (x y)
  (pointer-eq (pointer-to x) (pointer-to y)))


(defun id-equal (x y)
  ;; Note that we have to use INVOKE rather than PRIMITIVE-INVOKE here,
  ;; because we don't know wheter BOOL == char.  We don't even know
  ;; whether the typespec "c" indicates a char or an int, for that
  ;; matter (it only does so on NeXT/x86, but neither on GNUstep nor on
  ;; NeXT/ppc32).
  (or (id-eql x y)
      (truep (if (typep x '(or id objective-c-class exception))
                 (invoke x :is-equal y)
                 (progn
                   (assert (typep y '(or id objective-c-class exception)))
                   (invoke y :is-equal x))))))


(defun objc-typep (x class-designator)
  (objc-eql (object-get-class x)
            (etypecase x
              (class x)
              (id (object-get-class x))
              ((or string symbol) (find-objc-class class-designator t)))))


(defmethod objc-eql (x y)
  (cl:eql x y))

(defmethod objc-eql ((x id) y)
  (id-eql x y))

(defmethod objc-eql (x (y id))
  (id-eql x y))

(defmethod objc-eql ((x objective-c-class) y)
  (id-eql x y))

(defmethod objc-eql (x (y objective-c-class))
  (id-eql x y))

(defmethod objc-eql ((x exception) y)
  (id-eql x y))

(defmethod objc-eql (x (y exception))
  (id-eql x y))

(defmethod objc-eql ((x selector) (y selector))
  (eql (selector-name x) (selector-name y)))

(defmethod objc-eql ((x selector) (y string))
  (eql (selector-name x) y))

(defmethod objc-eql ((x string) (y selector))
  (eql x (selector-name y)))


(defmethod objc-equal (x y)
  (cl:equal x y))

(defmethod objc-equal ((x id) y)
  (id-equal x y))

(defmethod objc-equal (x (y id))
  (id-equal x y))

(defmethod objc-equal ((x objective-c-class) y)
  (id-equal x y))

(defmethod objc-equal (x (y objective-c-class))
  (id-equal x y))

(defmethod objc-equal ((x exception) y)
  (id-equal x y))

(defmethod objc-equal (x (y exception))
  (id-equal x y))

(defmethod objc-equal ((x selector) (y selector))
  (equal (selector-name x) (selector-name y)))

(defmethod objc-equal ((x selector) (y string))
  (equal (selector-name x) y))

(defmethod objc-equal ((x string) (y selector))
  (equal x (selector-name y)))


;;; (@* "Object Representation")
(defmethod print-object ((object id) stream)
  (with-slots (pointer) object
    (if (cffi:pointer-eq pointer (pointer-to +nil+))
        (format stream "#.~S" '+nil+)
        (print-unreadable-object (object stream)
          (format stream "~S `~A' {~X}"
                  (type-of object)
                  (primitive-invoke (primitive-invoke object "description" 'id)
                                    "UTF8String" :string)
                  (cffi:pointer-address pointer))))))


(defmethod print-object ((class objective-c-class) stream)
  (print-unreadable-object (class stream)
    (with-slots (pointer) class
      (format stream "~S ~S {~X}"
              (type-of class)
              (class-name class)
              (cffi:pointer-address pointer)))))


(defmethod print-object ((meta-class objective-c-meta-class) stream)
  (print-unreadable-object (meta-class stream)
    (with-slots (pointer) meta-class
      (format stream "~S ~S {~X}"
              (type-of meta-class)
              (class-name meta-class)
              (cffi:pointer-address pointer)))))


(defmethod print-object ((selector selector) stream)
  (print-unreadable-object (selector stream)
    (format stream "~S `~A'"
            (type-of selector)
            (selector-name selector))))


(defmethod print-object ((exception exception) stream)
  (cond (*print-escape*
         (print-unreadable-object (exception stream)
           ;; FIXME: Inexplicably, WITH-SLOTS doesn't work for
           ;; exceptions.  Is direct slot access disallowed for
           ;; instances of type CONDITION?
           (with-accessors ((pointer pointer-to)) exception
             (format stream "~S ~A {~X}"
                     (type-of exception)
                     (primitive-invoke (primitive-invoke exception "name" 'id)
                                       "UTF8String" :string)
                     (cffi:pointer-address pointer)))))
        (t (format stream "The Objective-C runtime has issued an exception ~
                           of type `~A'.~&~
                           Reason: ~A."
                   (invoke-by-name (invoke-by-name exception "name")
                                   "UTF8String")
                   (invoke-by-name (invoke-by-name exception "reason")
                                   "UTF8String")))))


(defmethod describe-object :after ((exception exception) stream)
  (format stream "Objective-C runtime type: ~S~&~
                  Reason: ~S"
          (invoke-by-name (invoke-by-name exception "name") "UTF8String")
          (invoke-by-name (invoke-by-name exception "reason") "UTF8String")))


(defmethod print-object ((struct foreign-struct) stream)
  (print-unreadable-object (struct stream)
    (with-slots (pointer name) struct
      (format stream "~S of type ~A {~X}"
              (type-of struct)
              name
              (cffi:pointer-address pointer)))))


;;; (@* "Structure and Union Definition")
(defun make-objc-struct/union-definer (type name-and-options c-names
                                       doc-and-slots)
  (let ((struct-name (ctypecase name-and-options
                       (list (first name-and-options))
                       (symbol name-and-options)))
        (slots (typecase (first doc-and-slots)
                 (string (rest doc-and-slots))
                 (t doc-and-slots))))
    `(progn
       ,(if (eq type :struct)
            `(defcstruct ,name-and-options ,@doc-and-slots)
            `(defcunion ,name-and-options ,@doc-and-slots))
       ,@(mapcar #'(lambda (slot)
                     (let ((slot-name (first slot)))
                       `(defun ,(intern (concatenate 'string
                                                     (symbol-name struct-name)
                                                     "-"
                                                     (symbol-name slot-name))
                                        (symbol-package slot-name))
                            (struct)
                          (check-type struct ,(if (eq type :struct)
                                                  `(or c-pointer
                                                       opaque-struct
                                                       tagged-struct)
                                                  `(or c-pointer
                                                       opaque-union
                                                       tagged-union)))
                          (when (typep struct ,(if (eq type :struct)
                                                   `'tagged-struct
                                                   `'tagged-union))
                            (assert (member (struct-name struct)
                                            ',c-names
                                            :test #'string=)))
                          (cffi:foreign-slot-value struct
                                                   ',struct-name
                                                   ',slot-name))))
                 slots))))

(defmacro define-objc-struct (name-and-options c-names &rest doc-and-slots)
  "Like CFFI:DEFCSTRUCT except that it provides accessors that check
their arguments according to their struct names."
  (make-objc-struct/union-definer :struct name-and-options c-names doc-and-slots))

(defmacro define-objc-union (name-and-options c-names &rest doc-and-slots)
  "Like CFFI:DEFCUNION except that it provides accessors that check
their arguments according to their struct names."
  (make-objc-struct/union-definer :union name-and-options c-names doc-and-slots))
