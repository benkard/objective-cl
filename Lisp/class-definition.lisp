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
   (foreign-offset :initarg :foreign-offset
                   :initform nil
                   :accessor slot-definition-foreign-offset)
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
            ;;? :location nil
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
          foreign-offset
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
          (parse-typespec (objcl-slot-type foreign-slot))))
  (unless foreign-offset
    (setf (slot-value slot-definition 'foreign-offset)
          (%objcl-get-slot-offset foreign-slot))))


(defmethod c2mop:slot-value-using-class ((class objective-c-class)
                                         instance
                                         (effective-slot-definition
                                          foreign-effective-slot-definition))
  (with-slots (foreign-name foreign-type foreign-offset) effective-slot-definition
     (convert-from-foreign-value (inc-pointer (pointer-to instance)
                                              foreign-offset)
                                 foreign-type
                                 nil
                                 t)))


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
    (with-slots (foreign-name foreign-type foreign-offset) effective-slot-definition
      (let* ((slot-cell (inc-pointer (pointer-to instance) foreign-offset)))
        (case (typespec-primary-type foreign-type)
          ((struct union array)
           (let ((value-pointer (typecase value
                                  (c-pointer value)
                                  (c-pointer-wrapper (pointer-to value)))))
             (memmove slot-cell value-pointer (sizeof foreign-type))))
          (otherwise
           ;; FIXME: What to do about memory management here?  Strings are
           ;; possibly the most problematic case.
           ;;
           ;; Also, should we do ID conversion as for method arguments
           ;; here?
           (setf (mem-ref slot-cell (typespec->c-type foreign-type)) value)))))))


(defmethod c2mop:slot-boundp-using-class ((class objective-c-class)
                                          instance
                                          (effective-slot-definition
                                           foreign-effective-slot-definition))
  (declare (ignore instance))
  t)


(defmethod c2mop:slot-makunbound-using-class ((class objective-c-class)
                                              instance
                                              (effective-slot-definition
                                               foreign-effective-slot-definition))
  (declare (ignore instance))
  (cerror "Continue without doing anything"
          "Tried to SLOT-MAKUNBOUND a foreign slot"))


(defmethod c2mop:compute-slots ((class objective-c-class))
  ;; FIXME: Maybe add lots of foreign slots here whose presence the
  ;; Objective-C runtime tells us.
  (call-next-method))


(defun ensure-objective-c-class-pair (name
                                      direct-superclasses
                                      direct-slots
                                      direct-default-initargs)
  (let* ((objective-c-superclasses
          (remove-if-not #'(lambda (c) (typep c 'objective-c-class))
                         direct-superclasses))
         (superclass
          (case (length objective-c-superclasses)
            (0 (find-objc-class "NSObject"))
            (1 (first objective-c-superclasses))
            (otherwise
             (error "Tried to derive all of ~S at the same time.  ~
                    (At most one Objective-C class may be derived at once.)"
                    objective-c-superclasses))))
         (ivars (remove-if-not #'(lambda (x)
                                   (getf x :foreign-type nil))
                               direct-slots))
         (new-class-pointer
          (objcl-create-class (symbol->objc-class-name name)
                              (find-if #'(lambda (c)
                                           (typep c 'objective-c-class))
                                       direct-superclasses)
                              nil
                              (mapcar #'(lambda (x)
                                          (getf x :foreign-name
                                                (slot-name->foreign-slot-name
                                                 (getf x :name))))
                                      ivars)
                              (mapcar #'(lambda (x)
                                          (getf x :foreign-type))
                                      ivars)))
         (metaclass
          (ensure-class (objc-meta-class-name->symbol
                         (symbol->objc-class-name name))
                        :metaclass (class-of (class-of superclass))
                        :pointer (%objcl-class-metaclass new-class-pointer)
                        :direct-superclasses (list (class-of superclass))
                        :new-foreign-class-p t))
         (class
          (ensure-class (intern (symbol-name name) '#:objective-c-classes)
                        :metaclass metaclass
                        :pointer new-class-pointer
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses
                        :direct-default-initargs direct-default-initargs
                        :new-foreign-class-p t)))
    (unless (eq (intern (symbol-name name) '#:objective-c-classes) name)
      (setf (find-class name) class)
      (setf (find-class (intern (symbol-name (class-name metaclass))))
            metaclass))
    (%objcl-class-set-backed-by-lisp-class new-class-pointer 1)
    class))


(defmethod make-instance ((class objective-c-meta-class)
                          &key pointer
                               name
                               direct-superclasses
                               direct-slots
                               direct-default-initargs
                          &allow-other-keys)
  (cond ((or (null pointer) (null-pointer-p pointer))
         ;; If we're creating a new Objective-C class,
         ;; (CALL-NEXT-METHOD) cannot possibly work, as the metaclass is
         ;; not yet in existence.  Therefore, we first cancel whatever
         ;; MAKE-INSTANCE is trying to do right now and take over from
         ;; here ourselves.
         ;;
         ;; Of course, ENSURE-OBJECTIVE-C-CLASS-PAIR is going to call
         ;; ENSURE-CLASS at some point, which will make MAKE-INSTANCE
         ;; run again, but this time with a sane metaclass already set
         ;; and ready to be instantiated.
         ;;
         ;; Note that this behaviour is (as far as I can tell)
         ;; compatible with what Clozure CL does.
         (let ((class
                (ensure-objective-c-class-pair name
                                               direct-superclasses
                                               direct-slots
                                               direct-default-initargs)))
           (setf (foreign-class-registered-p class) nil)
           class))
        (t (call-next-method))))


(defmethod reinitialize-instance ((class objective-c-class)
                                  &key &allow-other-keys)
  (call-next-method))


(defmethod initialize-instance :around ((class objective-c-class)
                                        &rest args
                                        &key pointer
                                             (new-foreign-class-p nil)
                                        &allow-other-keys)
  (cond ((not new-foreign-class-p)
         (let ((key-args (copy-list args)))
           ;; We scavenge the class and its superclasses for foreign
           ;; slots and add them to our :DIRECT-SLOTS keyword argument.
           (dolist (objc-slot (objcl-class-direct-slots/pointer pointer))
             (pushnew (list :name (foreign-slot-name->slot-name
                                   (objcl-slot-name objc-slot))
                            :foreign-name (objcl-slot-name objc-slot)
                            :foreign-type (parse-typespec (objcl-slot-type objc-slot)))
                      (getf key-args :direct-slots)
                      :key #'(lambda (slotd) (getf slotd :name))))
           (prog1 (apply #'call-next-method class key-args)
             (setf (foreign-class-registered-p class) t))))
        (t (call-next-method))))


(defmethod make-instance :before ((class objective-c-class)
                                  &key
                                  &allow-other-keys)
  (unless (subtypep class 'objective-c-meta-class)
    (foreign-class-ensure-registered class)))


(defun foreign-class-ensure-registered (class)
  (with-exclusive-access (class)
    (unless (foreign-class-registered-p class)
      (setf (foreign-class-registered-p class) t)
      (%objcl-finalise-class (pointer-to (class-of class)))
      (%objcl-finalise-class (pointer-to class))))
  class)


(defcallback collect-class :void ((class :pointer))
  (find-objc-class (%objcl-class-name class)))


(defun collect-classes ()
  (%objcl-for-each-class-do (callback collect-class)))
