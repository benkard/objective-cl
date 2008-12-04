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


(defmacro define-objective-c-class (name
                                    (&rest superclasses)
                                    (&rest slots)
                                    &body options)
  "Define a new Objective-C class.

## Arguments and Values:

*name* --- a **symbol**.

*superclasses* --- a **list** of **symbol**s (not evaluated).

*slots* --- a **list** (not evaluated).

*options* --- a **list** (not evaluated).

Returns: *class* --- the **class** just defined.


## Description:

_define-objective-c-class_ is like __defclass__ except in the following
aspects:

1. *name* is immediately replaced by a *symbol* **intern**ed in package
   _objective-c-classes_ (that is, the _ns_ namespace).

2. If *superclasses* is the **empty list**, a default value of
   _ns::ns-object_ will be used.

3. If the class does not exist yet, the default value for the
   _:metaclass_ option is _ns::+ns-object_.  Otherwise, the default
   value is the name of the current **metaclass** of the class.
   (Note that supplying a value different from _ns::+ns-object_ as the
   **metaclass** is usually not desirable.  See the note below for
   details.)

4. If any of the given *superclasses* in the _ns_ namespace is unknown
   at **load-time**, it will be assumed to name an __objective-c-class__
   and registered prior to execution of the __define-objective-c-class__
   form.

5. Slot specifications can specify foreign slots.  See below for
   details.

If *superclasses* contains more than one **symbol** that names an
__objective-c-class__ (or that is assumed to as per (4) above), an
__error__ will be **signal**led.

If *superclasses* is not the **empty list** but does not contain a
**symbol** that names an __objective-c-class__ (or that is assumed to as
per (4) above), the behaviour is undefined.


## Foreign Slot Specifier Syntax:

slot ::= (*slot-name* [[*slot-option* | *foreign-slot-option*]]\\*)

foreign-slot-option ::= \\{:foreign-type *typespec*\\} | \\{:foreign-name *foreign-name*\\}

*typespec* --- a **list** or a **symbol**.

*foreign-name* --- a **string**.


## Foreign Slot Specifier Description:

Foreign slot specifiers are like the standard slot specifiers that
__defclass__ recognises except that they also recognise the additional
options _:foreign-type_ and _:foreign-name_.

*typespec* is used to determine the C type the foreign slot will have.

If *typespec* is a **symbol**, it must be a CFFI type specifier, except
that the values _:id_ and _:class_ are also recognised.  If it is a
**list**, it has to be a valid *Objective-CL type specifier*, whose
syntax is not yet documented nor even finalised.

*foreign-name* is the name the slot will have on the Objective-C side.
It must be a valid Objective-C ivar identifier.

If *foreign-name* is omitted, it is derived from *slot-name* by
transforming it to Lower Camel Case as follows:

1. If the name is not in *canonical case*, it is left alone.

2. Otherwise, the name is split into parts separated by hyphens.

3. Each part except the first is capitalised and the results are
   concatenated onto the first part in order.


## Examples:

    (define-objective-c-class mlk-my-class ()
        ((foos :initargs :foos)
         (foo-count :foreign-type :int)))  ;foreign name will be \"fooCount\"
      => #<NS:+MLK-MY-CLASS NS:MLK-MY-CLASS {82c05c0}>

    (defvar \\*x\\* (invoke (find-objc-class 'mlk-my-class) 'new))
      => \\*X\\*

    (slot-boundp \\*x\\* 'foo-count)            => T
    (setf (slot-value \\*x\\* 'foo-count) 100)  => 100
    (slot-value \\*x\\* 'foo-count)             => 100

    (slot-boundp \\*x\\* 'foos)                        => NIL
    (setf (slot-value \\*x\\* 'foos) (list 'a 'b 'c))  => (A B C)
    (slot-value \\*x\\* 'foos)                         => (A B C)


## Note:

Regardless of the _:metaclass_ option given, the way _ns::+ns-object_ is
implemented will ensure that every __objective-c-class__ gets its very
own **metaclass** as in Objective-C.  The user is therefore strongly
discouraged from messing with the _:metaclass_ option.


## Note 2:

It is not an error to define **class**es of **type**
__objective-c-class__ in **package**s other than _objective-c-classes_,
but doing so will make some code behave in unexpected ways, so it is not
recommended.


## Note 3:

Foreign slots are always __slot-boundp__, as it is impossible to
determine whether they have been set on the Objective-C side or not.
Because of this, one has to be very careful when using _:string_ as the
*foreign-type*.


## See also:

  __defclass__, __define-objective-c-generic-function__,
__define-objective-c-method__"
  (let* ((name (intern (symbol-name name) '#:objective-c-classes))
         (objc-superclasses (remove-if-not
                             #'(lambda (c)
                                 (or (and (find-class c nil)
                                          (subtypep (find-class c) 'id))
                                     (and (not (find-class c nil))
                                          (eq (find-symbol (symbol-name name)
                                                           '#:objective-c-classes)
                                              name)
                                          name)))
                             superclasses))
         (objc-superclass (or (car objc-superclasses) 'ns::ns-object)))
    (assert (endp (cdr objc-superclasses)))
    `(prog2
       (find-objc-class ',objc-superclass)
       (defclass ,name ,(or superclasses '(ns::ns-object)) ,slots
         ,@(unless (member :metaclass options :key #'car)
             (if (find-class name nil)
                 `((:metaclass ,(class-name (class-of (find-class name)))))
                 `((:metaclass ns::+ns-object))))
         ,@options)
       (define-objective-c-method #:|retain| :id ((instance ,name))
         (super)
         (retain instance)
         instance)
       (define-objective-c-method #:|release| :void ((instance ,name))
         (release instance)
         (super)))))


(defclass foreign-slot-definition-mixin ()
  ((foreign-name :initarg :foreign-name
                 :initform nil
                 :accessor slot-definition-foreign-name
                 :type (or null string))
   (foreign-type :initarg :foreign-type
                 :initform nil
                 :type typespec
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
            :foreign-type (typespec foreign-type)
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


(defun ensure-objective-c-class-pair (name
                                      direct-superclasses
                                      direct-slots
                                      direct-default-initargs)
  (let* ((objective-c-superclasses
          (remove-if-not #'(lambda (c) (or (typep c 'objective-c-class)
                                           (eq (symbol-package (class-name c))
                                               (find-package '#:ns))))
                         direct-superclasses))
         (superclass
          (case (length objective-c-superclasses)
            (0 (find-objc-class "NSObject"))
            (1 (let ((class (first objective-c-superclasses)))
                 (if (typep class 'forward-referenced-class)
                     ;; Load the super class definition on demand.
                     (find-objc-class (symbol->objc-class-name (class-name class)))
                     class)))
            (otherwise
             (error "Tried to derive all of ~S at the same time.  ~
                    (At most one Objective-C class may be derived at once.)"
                    objective-c-superclasses))))
         (ivars (remove-if-not #'(lambda (x)
                                   (getf x :foreign-type nil))
                               direct-slots))
         (new-class-pointer
          (objcl-create-class (symbol->objc-class-name name)
                              superclass
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
  "Intern all __objective-c-class__es known to the runtime.

## Description:

__collect-classes__ makes all Objective-C classes known to the
Objective-C runtime available as __class__ metaobjects in the
_objective-c-classes_ **package** (that is, the _ns_ namespace).

Calling _collect-classes_ is optional.  It allows you to use
__find-class__ instead of __find-objc-class__, but this is purely an
aesthetic improvement.  _collect-classes_ is not necessary to use
Objective-CL productively.


## Note:

_collect-classes_ may take a very long time to complete depending on the
Lisp implementation.  Some implementations are not optimised to create
such a large number of classes along with their metaclasses on the fly.
If you care about portability, you may therefore want to do without the
extra convenience that _collect-classes_ offers.


## Note 2:

Even though when subclassing a foreign __class__, it needs to be
available as a __class__ metaobject in the _objective-c-classes_
package, you need not call _collect-classes_ before subclassing a
foreign __class__, because __define-objective-c-class__ takes care to
intern any missing superclass objects.


## See also:

  __collect-methods__"
  (%objcl-for-each-class-do (callback collect-class)))


;;;; (@* "Memory management")
(defvar *retained-lisp-objects* (make-hash-table :test #'eql))

;;; FIXME: Should we override #dealloc?  It may be the only way to get
;;; any information at all on a garbage-collected Objective-C runtime.

(defun retain (instance)
  ;; Ensure that INSTANCE is not garbage-collected on the Lisp side.
  (setf (gethash (pointer-address (pointer instance))
                 *retained-lisp-objects*)
        instance))

(defun release (instance)
  ;; If the reference count drops to 1, only the Lisp side has a
  ;; reference to the instance left.  (The Lisp side has certainly got
  ;; one because this very function does.)  In this case, we make the
  ;; Lisp wrapper object eligible for garbage collection by removing it
  ;; from the hash table.
  (when (<= (invoke-by-name instance "retainCount") 2)
    (remhash (pointer-address (pointer instance)) *retained-lisp-objects*)))
