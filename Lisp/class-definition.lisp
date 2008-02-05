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


(defvar *objcl-foreign-default-initform* (gensym))


(defclass foreign-slot-definition-mixin ()
  ((foreign-name :initarg :foreign-name
                 :initform nil
                 :accessor slot-definition-foreign-name
                 :type (or null string))
   (foreign-type :initarg :foreign-type
                 :initform nil
                 :accessor slot-definition-foreign-type)
   (foreign-slot :initarg :foreign-slot
                 :initform nil
                 :accessor slot-definition-foreign-slot)
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

(defmethod c2mop:validate-superclass ((class objective-c-class)
                                      (superclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class objective-c-class)
                                      (superclass objective-c-class))
  t)

(defmethod c2mop:direct-slot-definition-class ((class objective-c-class)
                                               &rest initargs)
  (if (some #'(lambda (symbol) (let ((nada '#:nada))
                                 (not (eq nada (getf initargs symbol nada)))))
            '(:foreign-type :foreign-name :foreign-slot))
      (find-class 'foreign-direct-slot-definition)
      (find-class 'c2mop:standard-direct-slot-definition)))


(defmethod c2mop:effective-slot-definition-class ((class objective-c-class)
                                                  &rest initargs)
  (if (some #'(lambda (symbol) (let ((nada '#:nada))
                                 (not (eq nada (getf initargs symbol nada)))))
            '(:foreign-type :foreign-name :foreign-slot))
      (find-class 'foreign-effective-slot-definition)
      (find-class 'c2mop:standard-effective-slot-definition)))


(defmethod c2mop:compute-effective-slot-definition ((class objective-c-class)
                                                    name
                                                    direct-slot-definitions)
  (etypecase (first direct-slot-definitions)
    (foreign-direct-slot-definition
     (let ((direct-slot (first direct-slot-definitions)))
       (with-accessors ((type c2mop:slot-definition-type)
                        (readers c2mop:slot-definition-readers)
                        (writers c2mop:slot-definition-writers)
                        (initargs c2mop:slot-definition-initargs)
                        (initform c2mop:slot-definition-initform)
                        (allocation c2mop:slot-definition-allocation)
                        (initfunction c2mop:slot-definition-initfunction)
                        (foreign-type slot-definition-foreign-type)
                        (foreign-name slot-definition-foreign-name)
                        (foreign-slot slot-definition-foreign-slot))
           direct-slot
         (make-instance 'foreign-effective-slot-definition
            :type (or type t)
            :name name
            :readers (or readers nil)
            :writers (or writers nil)
            :initargs (or initargs nil)
            :initform (or initform *objcl-foreign-default-initform*)
            :location nil
            :allocation (or allocation :instance)
            :initfunction (or initfunction
                              #'(lambda ()
                                  (or initform *objcl-foreign-default-initform*)))
            :foreign-type foreign-type
            :foreign-name foreign-name
            :foreign-slot foreign-slot
            :class class))))
    (c2mop:standard-direct-slot-definition (call-next-method))))


(defmethod initialize-instance :after
    ((slot-definition foreign-effective-slot-definition)
     &key foreign-name
          foreign-slot
          foreign-type
          name
          class
     &allow-other-keys)
  (when (and (not foreign-name) (not foreign-slot))
    (setf foreign-name (slot-name->foreign-slot-name name)
          (slot-value slot-definition 'foreign-name) foreign-name))
  (cond ((and foreign-name foreign-slot))
        (foreign-name
         (setf foreign-slot
               (or (find foreign-name
                         (mapcan #'objcl-class-direct-slots
                                 (c2mop:compute-class-precedence-list
                                  class))
                         :key #'objcl-slot-name
                         :test #'string=)
                   (error "There is no Objective-C slot named ~A in class ~A"
                          foreign-name
                          class))
               (slot-value slot-definition 'foreign-slot) foreign-slot))
        (foreign-slot
         (setf foreign-name (objcl-slot-name foreign-slot)
               (slot-value slot-definition 'foreign-name) foreign-name)))
  (unless foreign-type
    (setf (slot-value slot-definition 'foreign-type)
          foreign-slot)))


(defmethod c2mop:slot-value-using-class ((class objective-c-class)
                                         instance
                                         (effective-slot-definition
                                          foreign-effective-slot-definition))
  (with-slots (foreign-name foreign-type) effective-slot-definition
    (cffi:convert-from-foreign
     (%objcl-slot-value (pointer-to instance) foreign-name)
     (typespec->c-type foreign-type))))


(defmethod (setf c2mop:slot-value-using-class) (value
                                                (class objective-c-class)
                                                instance
                                                (effective-slot-definition
                                                 foreign-effective-slot-definition))
  ;; If we are directed to set the slot to the default initform dummy
  ;; value, we have probably been called during initialisation.  In this
  ;; case, do nothing.  There may or may not be useful information
  ;; present in the foreign slot at this time.
  (unless (eq value *objcl-foreign-default-initform*)
    (with-slots (foreign-name foreign-type) effective-slot-definition
      ;; FIXME: What to do about memory management here?  Strings are
      ;; possibly the most problematic case here.
      ;;
      ;; FIXME: This won't work at all right now, because
      ;; %OBJCL-SET-SLOT-VALUE expects a pointer to the value that it
      ;; should store as an argument, not the value itself.  For structs
      ;; and related things that can't be reasonably passed by value,
      ;; this is good news.  For everything else, it means just a bit
      ;; more work.
      (cerror "Do nothing" "FIXME")
      #+(or)
      (%objcl-set-slot-value
       instance
       foreign-name
       (cffi:convert-to-foreign value (typespec->c-type foreign-type))))))


(defmethod c2mop:slot-boundp-using-class ((class objective-c-class)
                                          instance
                                          (effective-slot-definition
                                           foreign-effective-slot-definition))
  (declare (ignore instance))
  t)


(defmethod c2mop:slot-makunbound-using-class ((class objective-c-class)
                                              instance
                                              effective-slot-definition)
  (declare (ignore instance))
  (etypecase effective-slot-definition
    (c2mop:standard-effective-slot-definition (call-next-method))
    (foreign-effective-slot-definition
     (cerror "Continue without doing anything"
             "Tried to SLOT-MAKUNBOUND a foreign slot"))))


(defmethod c2mop:compute-slots ((class objective-c-class))
  ;; FIXME: Maybe add lots of foreign slots here whose presence the
  ;; Objective-C runtime tells us.
  (call-next-method))


(defmethod initialize-instance :around ((class objective-c-class)
                                        &rest args
                                        &key pointer
                                        &allow-other-keys)
  ;; We scavenge the class and its superclasses for foreign slots and
  ;; add them to our :DIRECT-SLOTS keyword argument.
  (let ((key-args (copy-list args)))
    (dolist (objc-slot (objcl-class-direct-slots/pointer pointer))      
      (pushnew (list :name (intern (string-upcase (objcl-slot-name objc-slot))
                                   (find-package '#:objective-c-classes))
                     :foreign-name (objcl-slot-name objc-slot)
                     :foreign-type (parse-typespec (objcl-slot-type objc-slot)))
               (getf key-args :direct-slots)
               :key #'(lambda (slotd) (getf slotd :name))))
    (apply #'call-next-method class key-args)))


#+(or)
(defmethod make-instance ((class-name (eql 'objective-c-class)) &rest initargs)
  (let ((class (call-next-method)))
    class))

#+(or)
(defmethod c2mop:class-direct-superclasses ((class objective-c-class))
  (list (objcl-class-superclass class)))


#+(or)
(defmethod shared-initialize :after ((class objective-c-class)
                                     slot-names
                                     &rest initargs)
  )

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
  (:metaclass objective-c-classes::+ns-string)
  (:wrapped-foreign-class "NSString"))
