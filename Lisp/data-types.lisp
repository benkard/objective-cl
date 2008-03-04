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


;;;; (@* "Convenience types")
(deftype c-pointer ()
  '(satisfies pointerp))

(deftype argument-number ()
  `(integer 0 ,call-arguments-limit))


;;;; (@* "Foreign data types")
(defctype char-pointer :pointer)


;;;; (@* "Type specifiers")
(deftype objective-c-type-keyword ()
  `(member :id :class :selector :string
           :char :short :int :long :long-long
           :unsigned-char :unsigned-short :unsigned-int
           :unsigned-long :unsigned-long-long :pointer
           :float :double))


;;;; (@* "Objective-C object wrapper classes")
(with-compilation-unit ()  ; needed for class finalization
  (defclass c-pointer-wrapper ()
       ((pointer :type     c-pointer
                 :reader   pointer-to
                 :initarg  :pointer
                 :initform (cffi:null-pointer)))))


(defmethod make-load-form ((instance c-pointer-wrapper) &optional environment)
  (declare (ignore environment))
  ;; (TYPE-OF INSTANCE) works because MAKE-POINTER-WRAPPER accepts
  ;; subclasses of ID as well as ID itself.
  `(intern-pointer-wrapper ',(type-of instance)
                           :pointer (make-pointer
                                     ,(pointer-address (pointer-to instance)))))


;; The following may be needed by some implementations (namely Allegro
;; CL).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for class-name in '(c2mop:funcallable-standard-object
                            c-pointer-wrapper)
        for class = (find-class class-name nil)
        when class
          unless (c2mop:class-finalized-p class)
            do (c2mop:finalize-inheritance class)))


;; FIXME: I'm not confident about this, but it is needed in order to
;; make (DEFCLASS SELECTOR ...) work.
;;
;; On the other hand, CLISP's implementation notes specify this method
;; to return true by default for "some `obvious' cases" [29.12] such as
;; this one.  Therefore, we needn't override it.  In fact, we can't, at
;; least without disabling #<PACKAGE CLOS>'s package lock.
#-clisp
(defmethod c2mop:validate-superclass ((class c2mop:funcallable-standard-class)
                                      (superclass standard-class))
  t)


(defclass selector (c2mop:funcallable-standard-object c-pointer-wrapper)
     ()
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "An Objective-C method selector.

## Description:

Method selectors are Objective-C's equivalent to what Common Lisp calls
**symbols**.  Their use, however, is restricted to retrieving methods by
name.

In Common Lisp, you can **funcall** a __selector__ directly (see the
note below for details and why you may want to do this).

__selector__ objects cannot be created by means of __make-instance__.
Use __find-selector__ instead.


## Note:

Instead of using __invoke__, which is neither macro-friendly nor very
useful for method selection at run-time, you may **funcall** selectors
directly.  Naturally, __apply__ works as well.

The following calls are all equivalent:

    (invoke-by-name instance \"stringWithCString:encoding:\" \"Mulk.\" 4)
    (invoke instance :string-with-c-string \"Mulk.\" :encoding 4)
    (funcall (selector \"stringWithCString:encoding:\") instance \"Mulk.\" 4)
    (apply (selector \"stringWithCString:encoding:\") (list instance \"Mulk.\" 4))


## See also:

  __find-selector__"))


(defmethod shared-initialize :after ((selector selector)
                                     slot-names
                                     &rest initargs
                                     &key
                                     &allow-other-keys)
  (declare (ignore slot-names initargs))
  (c2mop:set-funcallable-instance-function
   selector
   #'(lambda (receiver &rest args)
       (apply #'invoke-by-name receiver selector args))))


(defmethod make-load-form ((selector selector) &optional environment)
  (declare (ignore environment))
  `(intern-pointer-wrapper 'selector
                           :pointer (make-pointer
                                     ,(pointer-address (pointer-to selector)))))


(defclass id (c-pointer-wrapper)
     ()
  (:documentation "The type of all Objective-C objects.

## Description:

The class __id__ is the supertype of all Objective-C instance types.  It
comprises all kinds of Objective-C objects that are instances of some
Objective-C class, that is, neither primitive C values nor __selector__,
__class__ or __exception__ objects.

__id__ objects cannot be created by means of __make-instance__.  Use
a suitable class method instead as you would in Objective-C.


## Examples:

    (invoke (find-objc-class 'ns-object)
            'self)
      ;=> #<NSObject `NSObject' {16ECF598}>

    (invoke (find-objc-class 'ns-string)
            :string-with-c-string \"Mulk.\")
      ;=> #<GSCBufferString `Mulk.' {5B36087}>

    (invoke (find-objc-class 'ns-string)
            'new)
      ;=> #<GSCBufferString `' {FFFFFFE}>


## See also:

  __invoke__, __invoke-by-name__, __exception__"))


(defclass objective-c-class (standard-class c-pointer-wrapper)
  ((registered-p :type boolean
                 :accessor foreign-class-registered-p
                 :documentation
                 "Whether the class has been registered with the Objective-C runtime.")))


(defclass objective-c-meta-class (objective-c-class)
     ((fake-p :type boolean
              :initform nil
              :initarg :fake-p
              :accessor metaclass-fake-p
              :documentation "Whether the class is a fake metaclass.")))


(define-condition exception (error)
    ((pointer :type     c-pointer
              :reader   pointer-to
              :initarg  :pointer))
  (:report (lambda (condition stream)
             (print-object condition stream)))
  (:documentation "The condition type for Objective-C exceptions.

## Description:

Whenever an Objective-C call made by means of __invoke__ or
__invoke-by-name__ raises an exception, the exception is propagated to
the Lisp side by being encapsulated in an __exception__ object and
signaled.

Note that it is currently impossible to directly extract the original
Objective-C exception from an __exception__ object, although it might
arguably be desirable to do so.  As __exception__ objects behave just
like __id__ objects in almost all circumstances, this is not much of a
problem, though.  If you really do need an __id__ instance rather than
an __exception__, you can simply send it the `self' message.


## Examples:

\(With __install-reader-syntax__ enabled.)

    (handler-case
        [NSArray selph]   ; oops, typo
      (exception (e)
        e))
      ;=> #<MULK.OBJECTIVE-CL:EXCEPTION NSInvalidArgumentException {1048D63}>

    (class-of *)
      ;=> #<CONDITION-CLASS EXCEPTION>

    (class-of [** self])
      ;=> #<STANDARD-CLASS ID>


## See also:

  __id__"))


(defclass foreign-value (c-pointer-wrapper)
     ((lisp-managed-cell :type (array boolean ())
                         :accessor foreign-value-lisp-managed-cell-p
                         :initarg :lisp-managed-cell
                         :documentation "Whether we need to handle deallocation.")))


;; FIXME: Document.
(defclass foreign-struct (foreign-value)
     ((name :type (or null string)
            :accessor foreign-struct-name
            :initarg :name)))


;; The following are for private use only.
(defclass opaque-struct (foreign-struct) ())

(defclass tagged-struct (foreign-struct)
     ((typespec :reader foreign-value-typespec
                :initarg :typespec)))

(defclass opaque-union (opaque-struct) ())

(defclass tagged-union (tagged-struct) ())


;; FIXME: Document.
(defgeneric foreign-value-lisp-managed-p (foreign-value))
(defmethod foreign-value-lisp-managed-p ((foreign-value foreign-value))
  (with-slots (lisp-managed-cell) foreign-value
    (aref lisp-managed-cell)))


;; FIXME: Document.
(defgeneric (setf foreign-value-lisp-managed-p) (managedp foreign-value))
(defmethod (setf foreign-value-lisp-managed-p)
    (managedp (foreign-value foreign-value))
  (with-slots (lisp-managed-cell) foreign-value
    (setf (aref lisp-managed-cell) (if managedp t nil))))


;; FIXME: Document.
(defgeneric foreign-value-pointer (foreign-value))
(defmethod foreign-value-pointer ((foreign-value foreign-value))
  (pointer-to foreign-value))


(defun make-struct-wrapper (pointer typespec managedp)
  ;; We use a zero-dimensional array that the finaliser can close over
  ;; so that it (the finaliser) can decide whether to garbage-collect
  ;; the foreign data.
  ;;
  ;; Using the instance slot directly would be both easier and more
  ;; transparent, of course, but it also wouldn't work, because during
  ;; finalisation, the instance is not in a usable state anymore.
  (let ((managedp-cell (make-array '() :element-type 'boolean
                                       :initial-element managedp)))
    (flet ((finaliser ()
             (when (aref managedp-cell)
               (foreign-free pointer))))
      (let ((new-wrapper (make-instance (ecase (typespec-primary-type typespec)
                                          (struct 'tagged-struct)
                                          (union 'tagged-union))
                            :name (third typespec)
                            :typespec typespec
                            :pointer pointer
                            :lisp-managed-cell managedp-cell)))
        (when managedp
          (trivial-garbage:finalize new-wrapper #'finaliser))))))


(defgeneric objcl-eql (obj1 obj2))
(defmethod objcl-eql ((obj1 c-pointer-wrapper) (obj2 c-pointer-wrapper))
  (pointer-eq (pointer-to obj1) (pointer-to obj2)))
(defmethod objcl-eql (obj1 obj2)
  (eql obj1 obj2))
