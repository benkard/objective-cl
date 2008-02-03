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


;;;; (@* "Objective-C object wrapper classes")
(with-compilation-unit ()  ; needed for class finalization
  (defclass c-pointer-wrapper ()
       ((pointer :type     c-pointer
                 :reader   pointer-to
                 :initarg  :pointer
                 :initform (cffi:null-pointer)))))


;; FIXME: I'm not confident about this, but it is needed in order to
;; make (DEFCLASS SELECTOR ...) work.
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


(defclass id (c-pointer-wrapper) ()
  (:documentation "An instance of an Objective-C class.

## Description:

The class __id__ serves as a general-purpose container for all kinds of
Objective-C objects that are instances of some Objective-C class, that
is, neither primitive C values nor __selector__, __class__ or
__exception__ objects.

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
  ())


(defclass objective-c-meta-class (objective-c-class)
  ())


(define-condition exception (error)
  ((pointer :type     c-pointer
            :accessor pointer-to
            :initarg  :pointer))
  (:report (lambda (condition stream)
             (format stream
                     "The Objective-C runtime has issued an exception of ~
                      type `~A'.~&~
                      Reason: ~A."
                     (invoke-by-name
                      (invoke-by-name condition "name")
                      "UTF8String")
                     (invoke-by-name
                      (invoke-by-name condition "reason")
                      "UTF8String"))))
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


(defclass opaque-struct (c-pointer-wrapper)
  ((name :type (or null string)
         :accessor struct-name
         :initarg :name)))

(defclass tagged-struct (c-pointer-wrapper)
  ((name :type (or null string)
         :accessor struct-name
         :initarg :name)
   (children :type list
             :accessor struct-children
             :initarg :children)))

(defclass opaque-union (c-pointer-wrapper)
  ((name :type (or null string)
         :accessor struct-name
         :initarg :name)))

(defclass tagged-union (c-pointer-wrapper)
  ((name :type (or null string)
         :accessor struct-name
         :initarg :name)
   (children :type list
             :accessor struct-children
             :initarg :children)))

(defclass tagged-array (c-pointer-wrapper)
  ((element-type :type symbol
                 :accessor tagged-array-element-type
                 :initarg :element-type)))


(defgeneric type-info (thing))

(defmethod type-info ((thing opaque-struct))
  (with-slots (name)
        thing
    (list* 'struct '(opaque) name)))

(defmethod type-info ((thing tagged-struct))
  (with-slots (name children)
        thing
    (list* 'struct '() name (mapcar #'type-info children))))


(defgeneric objcl-eql (obj1 obj2))
(defmethod objcl-eql ((obj1 c-pointer-wrapper) (obj2 c-pointer-wrapper))
  (pointer-eq (pointer-to obj1) (pointer-to obj2)))
(defmethod objcl-eql (obj1 obj2)
  (eql obj1 obj2))
