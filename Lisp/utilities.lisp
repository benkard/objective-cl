;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007  Matthias Andreas Benkard.
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(in-package #:mulk.objective-cl)


(defgeneric objc-eql (x y))
(defgeneric objc-equal (x y))


(defun truep (b)
  (or (eq b t)
      (and (numberp b)
           (not (eql b +no+)))))


(defun id-eql (x y)
  (pointer-eq (pointer-to x) (pointer-to y)))


(defun id-equal (x y)
  (or (id-eql x y)
      (truep (if (typep x '(or id objc-class exception))
                 (primitive-invoke x :is-equal :char y)
                 (progn
                   (assert (typep y '(or id objc-class exception)))
                   (primitive-invoke y :is-equal :char x))))))


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

(defmethod objc-eql ((x objc-class) y)
  (id-eql x y))

(defmethod objc-eql (x (y objc-class))
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

(defmethod objc-equal ((x objc-class) y)
  (id-equal x y))

(defmethod objc-equal (x (y objc-class))
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
          (format stream "~A `~A' {~X}"
                  (objc-class-name (primitive-invoke object "class" 'id))
                  (primitive-invoke (primitive-invoke object "description" 'id)
                                    "UTF8String" :string)
                  (cffi:pointer-address pointer))))))


(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (with-slots (pointer) class
      (format stream "~S ~A {~X}"
              (type-of class)
              (objc-class-name class)
              (cffi:pointer-address pointer)))))


(defmethod print-object ((meta-class objc-meta-class) stream)
  (print-unreadable-object (meta-class stream)
    (with-slots (meta-class-for-class pointer) meta-class
      (format stream "~S ~A {~X}"
              (type-of meta-class)
              (objc-class-name meta-class-for-class)
              (cffi:pointer-address pointer)))))


(defmethod print-object ((selector selector) stream)
  (print-unreadable-object (selector stream)
    (format stream "~S `~A'"
            (type-of selector)
            (selector-name selector))))


(defmethod print-object ((exception exception) stream)
  (print-unreadable-object (exception stream)
    (format stream "~S ~A {~X}"
            (type-of exception)
            (primitive-invoke (primitive-invoke exception "name" 'id)
                              "UTF8String" :string)
            (primitive-invoke exception "hash" :unsigned-int))))


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
