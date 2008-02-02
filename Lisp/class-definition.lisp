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


(defclass foreign-slot-definition-mixin ()
  ((foreign-name :initarg :foreign-name
                 :initform nil
                 :accessor slot-definition-foreign-name
                 :type (or null string))
   (foreign-type :initarg :foreign-type
                 :initform nil
                 :accessor slot-definition-foreign-type)
   #+#:unused
   (property :initarg :property
             :accessor slot-definition-property-p
             :type boolean)))

(defclass foreign-direct-slot-definition
     (foreign-slot-definition-mixin c2mop:standard-direct-slot-definition)
  ())

(defclass foreign-effective-slot-definition
     (foreign-slot-definition-mixin c2mop:standard-effective-slot-definition)
  ())


(defmethod c2mop:validate-superclass ((class objective-c-meta-class)
                                      (superclass standard-class))
  t)


(defmethod c2mop:direct-slot-definition-class ((class objective-c-class)
                                         &rest initargs)
  (if (some #'(lambda (symbol) (let ((nada '#:nada))
                                 (not (eq nada (getf initargs symbol nada)))))
            '(:foreign-type :foreign-name))
      (find-class 'foreign-direct-slot-definition)
      (find-class 'c2mop:standard-direct-slot-definition)))


(defmethod c2mop:effective-slot-definition-class ((class objective-c-class)
                                            &rest initargs)
  (if (some #'(lambda (symbol) (let ((nada '#:nada))
                                 (not (eq nada (getf initargs symbol nada)))))
            '(:foreign-type :foreign-name))
      (find-class 'foreign-effective-slot-definition)
      (find-class 'c2mop:standard-effective-slot-definition)))


(defmethod c2mop:slot-value-using-class ((class objective-c-class)
                                         instance
                                         effective-slot-definition)
  (etypecase effective-slot-definition
    (c2mop:standard-effective-slot-definition (call-next-method))
    (foreign-effective-slot-definition
     (cerror "FIXME" '()))))


(defmethod (setf c2mop:slot-value-using-class) (value
                                                (class objective-c-class)
                                                instance
                                                effective-slot-definition)
  (etypecase effective-slot-definition
    (c2mop:standard-effective-slot-definition (call-next-method))
    (foreign-effective-slot-definition
     (cerror "FIXME" '()))))


(defmethod c2mop:slot-boundp-using-class ((class objective-c-class)
                                          instance
                                          effective-slot-definition)
  (declare (ignore instance))
  (etypecase effective-slot-definition
    (c2mop:standard-effective-slot-definition (call-next-method))
    (foreign-effective-slot-definition t)))


(defmethod c2mop:compute-slots ((class objective-c-class))
  ;; FIXME: Maybe add lots of foreign slots here whose presence the
  ;; Objective-C runtime tells us.
  (call-next-method))


#+(or)
(defmethod make-instance ((class-name (eql 'objective-c-class)) &rest initargs)
  (let ((class (call-next-method)))
    class))

(defmethod initialize-instance ((class objective-c-class)
                                &key documentation
                                     name
                                     plist
                                     direct-superclasses
                                     direct-slots
                                     direct-default-initargs
                                     pointer
                                     wrapped-foreign-class)
  (call-next-method))

(defmethod reinitialize-instance ((class objective-c-class)
                                  &key documentation
                                       name
                                       plist
                                       direct-superclasses
                                       direct-slots
                                       direct-default-initargs
                                       pointer
                                       wrapped-foreign-class)
  (call-next-method))

(defmethod initialize-instance ((class objective-c-meta-class)
                                &key documentation
                                     name
                                     plist
                                     direct-superclasses
                                     direct-slots
                                     direct-default-initargs
                                     pointer)
  (call-next-method))

(defmethod reinitialize-instance ((class objective-c-meta-class)
                                  &key documentation
                                       name
                                       plist
                                       direct-superclasses
                                       direct-slots
                                       direct-default-initargs
                                       pointer)
  (call-next-method))

#+(or)
(defmethod c2mop:compute-effective-slot-definition ((class objective-c-class)
                                                    slot-name
                                                    direct-slots)
  (call-next-method))


#+(or)
(make-instance 'objective-c-class :wrapped-foreign-class "NSString")
#+(or)
(c2mop:ensure-class 'ns-string
                    :metaclass 'objective-c-class
                    :wrapped-foreign-class "NSString")
#+(or)
(defclass ns-string ()
  ((bla :foreign-type 'string
        :foreign-name "_bla"
        :accessor ns-string-bla)
   (nothing :accessor ns-string-nothing
            :initarg :ns-string
            :initform 100))
  (:metaclass objective-c-classes::%ns-string)
  (:wrapped-foreign-class "NSString"))
