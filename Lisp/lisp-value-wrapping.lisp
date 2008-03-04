;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007, 2008  Matthias Andreas Benkard.
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (find-objc-class "NSObject" t)
  (find-objc-class "NSString" t)
  (find-objc-class "NSArray" t)
  (find-objc-class "NSDictionary" t))


(defclass lisp-value-wrapper-mixin ()
     ((lisp-value :initarg :value
                  :initform nil
                  :accessor lisp-value)))


;; May usefully override, among others:
;;  - description
(defclass ns::mlk-lisp-value (ns::ns-object lisp-value-wrapper-mixin)
     ()
  #+(or) (:default-constructor new)
  (:metaclass ns::+ns-object))


(defcoercion id ((x list))
  (intern-lisp-value x))

(defcoercion id ((x string))
  ;; FIXME: Implement INTERN-LISP-VALUE.
  (primitive-invoke (find-objc-class 'ns-string)
                    "stringWithUTF8String:"
                    'id
                    x))

(defcoercion id ((x t))
  (intern-lisp-value x))


(defun intern-lisp-value (value)
  ;; We need this function in order to preserve object identity on the
  ;; Objective-C side.  As we want [(intern-lisp-value 10) self] to
  ;; return the FIXNUM 10, that is, a Lisp value rather than an
  ;; Objective-C instance, we cannot guarantee that
  ;;
  ;;  (let ((x (intern-lisp-value y)))
  ;;    (objc-eql x (invoke x 'self)))
  ;;
  ;; will evaluate to true unless we generally intern Lisp value
  ;; wrappers.
  (error "FIXME"))


(defun make-lisp-value (value)
  ;; FIXME: The following won't work.  Make MAKE-INSTANCE more useful...
  ;(make-instance 'ns::mlk-lisp-value :value value)
  (let ((instance (invoke (typecase value
                            (string (find-class 'ns::mlk-lisp-string))
                            (vector (find-class 'ns::mlk-lisp-array))
                            (list (find-class 'ns::mlk-lisp-list))
                            (t (find-class 'ns::mlk-lisp-value)))
                          'new)))
    (setf (lisp-value instance) value)
    instance))


;; Must override:
;;  - characterAtIndex:
;;  - length
;;
;; May usefully override, among others:
;;  - substringWithRange: (maybe)
;;  - getCharacters:range: (for performance reasons)
;;  - description
(defclass ns::mlk-lisp-string (ns::ns-string lisp-value-wrapper-mixin)
     ()
  (:metaclass ns::+ns-object))


;; Must override:
;;  - objectAtIndex:
;;  - count
;;
;; May usefully override, among others:
;;  - description
(defclass ns::mlk-lisp-array (ns::ns-array lisp-value-wrapper-mixin)
     ()
  (:metaclass ns::+ns-object))


;; Must override:
;;  - objectAtIndex:
;;  - count
;;
;; May usefully override, among others:
;;  - description
(defclass ns::mlk-lisp-list (ns::ns-array lisp-value-wrapper-mixin)
     ()
  (:metaclass ns::+ns-object))
