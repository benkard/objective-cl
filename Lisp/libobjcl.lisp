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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew objcl-asdf:*objc-obj-dir* cffi:*foreign-library-directories*)
  (dolist (subdir '("shared_obj/" "obj/"))
    (pushnew (merge-pathnames subdir objcl-asdf:*objc-obj-dir*)
             cffi:*foreign-library-directories*)))

(define-foreign-library libobjcl
  (:unix (:or "libobjcl.so"
              "libobjcl.so.0"
              "libobjcl.dylib"
              "libobjcl.dylib.0"))
  (t (:default "libobjcl")))

(use-foreign-library libobjcl)


(defcfun ("objcl_memmove" memmove) :pointer
  (destination :pointer)
  (source :pointer)
  (length :unsigned-long))

(defcfun ("objcl_initialise_runtime" %initialise-runtime) :void)

(defcfun ("objcl_shutdown_runtime" %shutdown-runtime) :void)

(defcfun ("objcl_invoke_with_types" %objcl-invoke-with-types) :pointer
  (argc :int)
  (superclass-for-send-super :pointer)
  (return-typespec :string)
  (arg-typespecs (:array :string))
  (return-value (:pointer :void))
  (argv (:array (:pointer :void))))

(defcfun ("objcl_find_class" %objcl-find-class) :pointer
  (class-name :string))

(defcfun ("objcl_find_meta_class" %objcl-find-meta-class) :pointer
  (class-name :string))

(defcfun ("objcl_class_name" %objcl-class-name) :string
  (class :pointer))

(defcfun ("objcl_class_superclass" %objcl-class-superclass) :pointer
  (obj :pointer))

(defcfun ("objcl_class_metaclass" %objcl-class-metaclass) :pointer
  (obj :pointer))

(defcfun ("objcl_find_selector" %objcl-find-selector) :pointer
  (selector-name :string))

(defcfun ("objcl_intern_selector" %objcl-intern-selector) :pointer
  (selector-name :string))

(defcfun ("objcl_selector_name" %objcl-selector-name) :string
  (selector :pointer))

(defcfun ("objcl_get_method_implementation"
          %objcl-get-method-implementation)
    :pointer
  (object :pointer)
  (selector :pointer))

(defcfun ("objcl_object_is_class" %objcl-object-is-class) :boolean
  (obj :pointer))

(defcfun ("objcl_object_is_meta_class" %objcl-object-is-meta-class)
    :boolean
  (obj :pointer))

(defcfun ("objcl_object_get_class" %objcl-object-get-class) :pointer
  (obj :pointer))

(defcfun ("objcl_object_get_meta_class" %objcl-object-get-meta-class)
    :pointer
  (obj :pointer))

(defcfun ("objcl_get_runtime_type" %objcl-get-runtime-type) :string)

(defcfun ("objcl_objc2_p" %objcl-objc2-p) :int)

(defcfun ("objcl_sizeof_type" %objcl-sizeof-type) :long
  (typespec :string))

(defcfun ("objcl_sizeof_return_type" %objcl-sizeof-return-type) :long
  (typespec :string))

(defcfun ("objcl_alignof_type" %objcl-alignof-type) :long
  (typespec :string))

(defcfun ("objcl_set_slot_value" %objcl-set-slot-value) :void
  (obj :pointer) ; id
  (slot-name :string)
  (value :pointer)) ; void

(defcfun ("objcl_get_slot_value" %objcl-get-slot-value) :void
  (obj :pointer) ; id
  (slot-name :string)
  (value-out :pointer))

(defcfun ("objcl_get_slot" %objcl-get-slot) :pointer
  (class :pointer) ; Class
  (slot-name :string))

(defcfun ("objcl_get_slot_offset" %objcl-get-slot-offset) :long
  (slot :pointer))

(defcfun ("objcl_class_direct_slots" %objcl-class-direct-slots) :pointer
  (class :pointer) ; Class
  (count :pointer) ; unsigned int
  (element-size :pointer)) ; unsigned int

(defcfun ("objcl_slot_name" %objcl-slot-name) :string
  (slot :pointer)) ; IVAR_T

(defcfun ("objcl_slot_type" %objcl-slot-type) :string
  (slot :pointer)) ; IVAR_T

(defcfun ("objcl_get_nil" %objcl-get-nil) :pointer)
(defcfun objcl-get-yes :long)
(defcfun objcl-get-no :long)

(defcfun ("objcl_create_imp" %objcl-create-imp) :pointer
  (callback :pointer)
  (argc :int)
  (return-typespec :string)
  (arg-typespecs (:array :string)))

(defcfun ("objcl_acquire_lock" %objcl-acquire-lock) :pointer
  (lock :pointer))

(defcfun ("objcl_release_lock" %objcl-release-lock) :pointer
  (lock :pointer))

(defcfun ("objcl_create_class" %objcl-create-class) :pointer
  (class-name :string)
  (superclass :pointer)
  (protocol-numer :int)
  (protocol-names (:array :string))
  (ivar-number :int)
  (ivar-names (:array :string))
  (ivar-typespecs (:array :string)))

(defcfun ("objcl_add_method" %objcl-add-method) :void
  (class :pointer)
  (class-name :string)
  (method-name :pointer)
  (callback :pointer)
  (argc :int)
  (return-typespec :string)
  (arg-typespecs (:array :string))
  (signature :string)
  (registered-p :int))

(defcfun ("objcl_finalise_class" %objcl-finalise-class) :void
  (class :pointer))

(defcfun ("objcl_class_backed_by_lisp_class_p"
          %objcl-class-backed-by-lisp-class-p)
    :int
  (class :pointer))

(defcfun ("objcl_class_set_backed_by_lisp_class"
          %objcl-class-set-backed-by-lisp-class)
    :void
  (class :pointer)
  (backed-p :int))

(defcfun ("objcl_object_backed_by_lisp_class_p"
          %objcl-object-backed-by-lisp-class-p)
    :int
  (instance :pointer))

(defcfun ("objcl_for_each_class_do" %objcl-for-each-class-do)
    :int
  (function :pointer))

(defcfun ("objcl_test_foo" objcl-test-foo) :void)

(defcfun ("objcl_method_selector" %objcl-method-selector) :pointer
  (method :pointer))

(defcfun ("objcl_class_methods" %objcl-class-methods) :pointer
  (class :pointer)
  (count-out :pointer))

(defcvar *objcl-current-exception-lock* :pointer)
(defcvar *objcl-current-exception* :pointer)

(defun objcl-get-nil ()
  ;; %OBJCL-GET-NIL can return NIL for CLISP, which CFFI refuses to
  ;; accept as an argument to POINTER-EQ.  This is weird.
  (or (%objcl-get-nil) (make-pointer 0)))

(defun initialise-runtime ()
  "Initialise the Objective-C runtime.

## Description:

The function __initialise-runtime__ makes all the necessary arrangements
for object instantiation and method calls to work.  In particular, it
creates an autorelease pool in order to make the use of Objective-C's
semiautomatic reference counting memory management possible, which is
used internally by Objective-CL.

Note that, as the autorelease pool created by __initialise-runtime__ is
currently only deallocated and its containees released when
__shutdown-runtime__ is called, it is generally advisable to make use of
AppKit's automatic creation and deletion auf autorelease pools, if
possible.  Naturally, AppKit-based applications need not worry about
this, but be aware that they do need to call __initialise-runtime__
before making any other Objective-C calls.


## See also:

  __shutdown-runtime__"

  (when (zerop *runtime-initialisation-level*)
    (%initialise-runtime))
  (atomically (incf *runtime-initialisation-level*)))


(defun shutdown-runtime ()
  "Shut the Objective-C runtime down.

## Description:

The function __shutdown-runtime__ cleans the environment up.  In
particular, it tries to release all the objects retained in any
autorelease pools created by __initialise-runtime__.

Note that even if you make use of AppKit, which manages its own
autorelease pools, you must call __initialise-runtime__ before making
any Objective-C calls, and you should call __shutdown-runtime__ when you
are finished with Objective-C, since Objective-CL makes use of
autoreleased objects internally before you are even able to retrieve any
objects or classes, let alone send messages to them.


## See also:

  __initialise-runtime__"

  (when (zerop (atomically (decf *runtime-initialisation-level*)))
    (%shutdown-runtime)))


(declaim (ftype (function ((or string symbol) &optional t)
                          (or null objective-c-class))
                find-objc-class))
(defun find-objc-class (class-name &optional errorp)
  "Retrieve an Objective-C class by name.

## Arguments and Values:

*class-name* --- a **symbol** or a **string**.

*errorp* --- a **generalized boolean**.

Returns: *class* --- an __objc-class__ object representing the Objective
C class whose name is *class-name*.


## Description:

If no Objective-C class named by *class-name* is found, the behaviour
depends on *errorp*: If *errorp* is **true**, an error is signaled.  If
*errorp* is **false** (which is the default), __nil__ is returned.

If *class-name* is a **symbol** which does not contain a hyphen, its
**name** is converted to **lowercase** except for the first letter,
which is left intact, and the resulting **string** used as if directly
given as an **argument** to __find-objc-class__.

If *class-name* is a **symbol** which contains a hyphen, its **name**
is split into components separated by hyphens and each component
converted into a **string** according to the following rules:

1. The first component is fully converted to **uppercase** except for
   its first letter, which is left intact.

2. Any additional components have all of their letters converted to
   **lowercase**, except for their first letters, which are left intact.

After that, the components are concatenated in order and the resulting
**string** used as if directly given as an **argument** to
__find-objc-class__.


## Examples:

    (find-objc-class \"NSObject\")     ;=> #<OBJC-CLASS NSObject>
    (find-objc-class 'ns-object)     ;=> #<OBJC-CLASS NSObject>
    (find-objc-class 'nsobject)      ;=> NIL


## Rationale:

The first component of an Objective-C class name is conventionally
thought of as a namespace identifier.  It is therefore sensible to
expect it to be converted to **uppercase** by default, which is the
conventional case for namespace identifiers in Objective-C."

  (let ((class
         (etypecase class-name
           (string (find-objc-class-by-name class-name))
           (symbol (find-objc-class-by-name
                    (symbol->objc-class-name class-name))))))
    (or class (if errorp
                  (error "Found no Objective-C class named ~S."
                         class-name)
                  nil))))


(declaim (ftype (function (string) (or null objective-c-class))
                find-objc-class-by-name))
(defun find-objc-class-by-name (class-name-string)
  ;; Note that this function is able to find classes that are not yet
  ;; registered with the Objective-C runtime simply by looking for an
  ;; extant CLOS class definition.
  ;;
  ;; This is important, as FIND-OBJC-CLASS-BY-NAME is called more often
  ;; than you might think.  For instance, doing a (PRIMITIVE-INVOKE
  ;; class ...) will cause (OBJECT-GET-CLASS class) to be called at some
  ;; point, which would otherwise fail.  Of course, doing a method
  ;; invocation on the class will cause the class to be registered along
  ;; the way as well.
  (let ((class-name (objc-class-name->symbol class-name-string))
        (class-ptr (%objcl-find-class class-name-string)))
    (or (find-class class-name nil)
        (if (objc-pointer-null class-ptr)
            nil
            (let ((superclass (let ((potential-superclass
                                     (objcl-class-superclass/pointer class-ptr)))
                                (if potential-superclass
                                    (foreign-class-ensure-registered
                                     potential-superclass)
                                    (find-class 'id)))))
              (c2mop:ensure-class class-name
                                  :metaclass (class-name
                                              (find-objc-meta-class
                                               class-name-string))
                                  :pointer class-ptr
                                  ;; FIXME: It might be desirable to do
                                  ;; set the list of direct superclasses
                                  ;; to something like the following:
                                  ;;
                                  ;;  (if (string= class-name-string "NSException")
                                  ;;      (list superclass (find-class 'serious-condition))
                                  ;;      (list superclass))
                                  ;;
                                  ;; This will fail, as subclassing
                                  ;; CONDITION means not subclassing a
                                  ;; STANDARD-CLASS.  Also, care would
                                  ;; have to be taken not to use
                                  ;; MAKE-INSTANCE but MAKE-CONDITION
                                  ;; for anything derived from
                                  ;; NSException.
                                  :direct-superclasses (list superclass)))))))


(defun find-objc-meta-class (meta-class-name &optional errorp)
  (let ((meta-class
         (etypecase meta-class-name
           (string (find-objc-meta-class-by-name meta-class-name))
           (symbol (find-objc-meta-class-by-name
                    (symbol->objc-class-name meta-class-name))))))
    (or meta-class (if errorp
                       (error "Found no Objective-C metaclass named ~S."
                              meta-class-name)
                       nil))))


(defun find-objc-meta-class-by-name (class-name-string)
  (let ((class-ptr (%objcl-find-meta-class class-name-string)))
    (if (objc-pointer-null class-ptr)
        nil
        (let* ((class-name (objc-meta-class-name->symbol class-name-string))
               ;; We first determine the superclass of the class that
               ;; this metaclass is the metaclass of, because we cannot
               ;; determine a metaclass' superclass directly.
               (non-meta-superclass (objcl-class-superclass/pointer
                                     (%objcl-find-class class-name-string)))
               ;; If such a non-meta-superclass is found, we look for a
               ;; metaclass with the same name and take that as our
               ;; superclass.  Otherwise, OBJECTIVE-C-CLASS should be
               ;; quite the most correct choice.
               (superclass (if non-meta-superclass
                               (find-objc-meta-class
                                (%objcl-class-name
                                 (pointer-to
                                  (foreign-class-ensure-registered
                                   non-meta-superclass))))
                               (find-class 'objective-c-meta-class)))
               ;; If there is no superclass, we are the root metaclass.
               ;; As we cannot assign ourselves as our own metaclass
               ;; (which is a pity, because it would be the correct thing
               ;; to do), we generate a fake metaclass for ourselves that
               ;; is almost the same as ourselves except for its own
               ;; metaclass, which is OBJECTIVE-C-META-CLASS.  (As we are
               ;; probably +NS-OBJECT, this fake metaclass will be called
               ;; ++NS-OBJECT.)
               ;;
               ;; If the superclass is the root metaclass, we take it as
               ;; our metaclass, because the root metaclass is the
               ;; metaclass of all metaclasses.
               ;;
               ;; Otherwise, we use the metaclass of the superclass as
               ;; our own, which will always get us the root metaclass.
               (metaclass
                (if non-meta-superclass
                    (if (metaclass-fake-p (class-of superclass))
                        superclass
                        (class-of superclass))
                    (let ((fake-metaclass-name
                           (objc-fake-meta-class-name->symbol class-name-string)))
                      (or (find-class fake-metaclass-name nil)
                          (c2mop:ensure-class fake-metaclass-name
                                              :metaclass 'objective-c-meta-class
                                              :pointer class-ptr
                                              :direct-superclasses (list superclass)
                                              :fake-p t))))))
          (or (find-class class-name nil)
              (c2mop:ensure-class class-name
                                  :metaclass metaclass
                                  :pointer class-ptr
                                  :direct-superclasses (list superclass)))))))


(defun objc-pointer-null (pointer)
  (or (cffi:null-pointer-p pointer)
      (cffi:pointer-eq pointer (objcl-get-nil))))


(defun objc-null (value)
  (or (null value)
      (and (typep value 'c-pointer-wrapper)
           (objc-pointer-null (pointer-to value)))))


(defmacro objc-or (&rest forms)
  (let ((sym (gensym)))
    `(let ((,sym ,(first forms)))
       (if (objc-null ,sym)
           ,(if (rest forms)
                `(objc-or ,@(rest forms))
                'nil)
           ,sym))))


(declaim (ftype (function (string) (or null selector))
                find-selector-by-name))
(defun find-selector-by-name (selector-name)
  (let ((selector-ptr (%objcl-find-selector selector-name)))
    (if (cffi:null-pointer-p selector-ptr)
        nil
        (intern-pointer-wrapper 'selector :pointer selector-ptr))))


(defun intern-selector-by-name (selector-name)
  (let ((selector-ptr (%objcl-intern-selector selector-name)))
    (assert (not (cffi:null-pointer-p selector-ptr))
            (selector-ptr)
            "%OBJCL-INTERN-SELECTOR must always return a selector.")
    (intern-pointer-wrapper 'selector :pointer selector-ptr)))


(declaim (ftype (function ((or objective-c-class id exception)) string)
                objc-class-name))
(defun objc-class-name (class)
  "Find the name of a class.

## Arguments and Values:

*class* --- an **object** of **type** __objc-id__ or __objc-class__.

Returns: *name* --- a **string**.


## Description:

__objc-class-name__ returns the name of *class*.


## Examples:

    (objc-class-name (find-objc-class 'ns-object))  ;=> \"NSObject\"


## Note:

If *x* is an **object** of **type** __objc-id__ or __objc-class__:

    (objc-eql x (find-objc-class (objc-class-name x)))  ;=> T

If *name* is the name of an existing class:

    (equal name (objc-class-name (find-objc-class name)))  ;=> T


## See Also:

  __find-objc-class__"
  (declare (type (or objective-c-class id exception) class))
  (%objcl-class-name (pointer-to class)))


(declaim (ftype (function (selector) string) selector-name))
(defun selector-name (selector)
  "Find the name of a selector.

## Arguments and Values:

*selector* --- an **object** of **type** __selector__.

Returns: *name* --- a **string**.


## Description:

__selector-name__ returns the name of *selector*.


## Examples:

    (selector-name (selector '(:string-with-c-string :encoding)))
     ;=> \"stringWithCString:encoding:\"


## Note:

If *x* is an **object** of **type** __selector__:

    (objc-equal x (find-selector (selector-name x)))  ;=> T

If *name* is the name of an existing selector:

    (equal name (selector-name (find-selector name)))  ;=> T


## See Also:

  __find-selector__, __selector__"
  (declare (type selector selector))
  (%objcl-selector-name (pointer-to selector)))


(declaim (ftype (function ((or id objective-c-class exception) selector) t)
                get-method-implementation))
(defun get-method-implementation (object selector)
  (declare (type selector selector))
  (%objcl-get-method-implementation (pointer-to object)
                                    (pointer-to selector)))


(declaim (ftype (function ((or selector symbol string list) &optional t)
                          (or null selector))
                find-selector))
(defun find-selector (selector-name &optional (errorp t))
  "Retrieve a method selector by name.

## Arguments and Values:

*selector-name* --- a **string**, a **symbol**, or a **list** of **symbol**s.

*errorp* --- a **generalized boolean**.  The default is **true**.

Returns: *selector* --- a __selector__ object, or __nil__.


## Description:

If *selector-name* is a **string**, the __selector__ named by that
string is returned.  If no __selector__ with the given name exists,
either __nil__ is returned if errorp is **false**, or an error of type
__no-such-selector__ is signaled if errorp is **true**.

If *selector-name* is a **symbol**, it is treated the same as a **list**
whose only element is the **symbol**.

If *selector-name* is a **list** of **symbol**s, all **symbol**s are
first split into parts separated by hyphens and each part converted into
a **string** according to the following rules:

1. 1. If the keywords' **symbol name**s do contain **lowercase**
      **character**s, their case is left intact.

   2. If the keywords' **symbol name**s do not contain any **lowercase**
      **character**s, the following steps are taken in order to adjust
      their case.

      1. The first part is fully converted to **lowercase**.

      2. Any additional parts are also fully converted to **lowercase**
         except for their first letters, which are left intact.

2. If the symbol is a **keyword**, the resulting **string** is suffixed
   by a **colon** (`:').

After that, all parts are concatenated in order to form a
single *selector name component*.  The *selector name components* are in
turn concatenated in order to form the **string** that identifies the
selector, which is used as if given directly as an argument to a call to
__find-selector__.

Note that the conversion rules for selector names are identical to those
by which __invoke__ converts its arguments into a *message name*.


## Examples:

    (find-selector \"self\")       ;=> #<SELECTOR `self'>
    (find-selector '(self))      ;=> #<SELECTOR `self'>
    (find-selector 'self)        ;=> #<SELECTOR `self'>

    (find-selector \"stringWithCString:encoding:\")
      ;=> #<SELECTOR `stringWithCString:encoding:'>

    (find-selector '(:string-with-c-string :encoding))
      ;=> #<SELECTOR `stringWithCString:encoding:'>

    #.(setq \\*readtable\\* (copy-readtable)) 
    #.(setf (readtable-case \\*readtable\\*) :invert)
    (find-selector '(:stringWithCString :encoding))
      ;=> #<SELECTOR `stringWithCString:encoding:'>


## Note:

Setting the **readtable case** of the **current readtable** to `:INVERT`
is a good way of making the Lisp system behave as traditionally as
possible while making Objective-C method names case-sensitive.

On the other hand, writing all method names in lower case while
separating parts by hyphens works nicely in all of the `:INVERT`,
`:UPCASE`, `:DOWNCASE`, and `:PRESERVE` modes as well as Allegro CL's
*modern mode*.


## See also:

  __intern-selector__, __selector__"

  (or (typecase selector-name
        (string (find-selector-by-name selector-name))
        (symbol (find-selector-by-name (symbol-list->message-name
                                        (list selector-name))))
        (list   (find-selector-by-name (symbol-list->message-name
                                        selector-name))))
      (and errorp
           (error (make-condition 'no-such-selector :designator selector-name)))))


(defun intern-selector (selector-name)
  "Retrieve a method selector by name, or create it if it does not exist.

## Arguments and Values:

*selector-name* --- a **string**, a **symbol**, or a **list** of **symbol**s.

Returns: *selector* --- a __selector__ object.


## Description:

If *selector-name* is a **string**, the __selector__ named by that
string is returned.  If no __selector__ with the given name exists, such
a selector is created and registered with the Objective-C runtime, after
which it is returned.

If *selector-name* is a **symbol**, it is treated the same as a **list**
whose only element is the **symbol**.

If *selector-name* is a **list** of **symbol**s, all **symbol**s are
first split into parts separated by hyphens and each part converted into
a **string** according to the following rules:

1. 1. If the keywords' **symbol name**s do contain **lowercase**
      **character**s, their case is left intact.

   2. If the keywords' **symbol name**s do not contain any **lowercase**
      **character**s, the following steps are taken in order to adjust
      their case.

      1. The first part is fully converted to **lowercase**.

      2. Any additional parts are also fully converted to **lowercase**
         except for their first letters, which are left intact.

2. If the symbol is a **keyword**, the resulting **string** is suffixed
   by a **colon** (`:').

After that, all parts are concatenated in order to form a
single *selector name component*.  The *selector name components* are in
turn concatenated in order to form the **string** that identifies the
selector, which is used as if given directly as an argument to a call to
__intern-selector__.

Note that the conversion rules for selector names are identical to those
by which __invoke__ converts its arguments into a *message name*.


## Examples:

    (intern-selector \"self\")       ;=> #<SELECTOR `self'>
    (intern-selector '(self))      ;=> #<SELECTOR `self'>
    (intern-selector 'self)        ;=> #<SELECTOR `self'>

    (intern-selector \"stringWithCString:encoding:\")
      ;=> #<SELECTOR `stringWithCString:encoding:'>

    (intern-selector '(:string-with-c-string :encoding))
      ;=> #<SELECTOR `stringWithCString:encoding:'>

    #.(setq \\*readtable\\* (copy-readtable)) 
    #.(setf (readtable-case \\*readtable\\*) :invert)
    (intern-selector '(:stringWithCString :encoding))
      ;=> #<SELECTOR `stringWithCString:encoding:'>


## Note:

Setting the **readtable case** of the **current readtable** to `:INVERT`
is a good way of making the Lisp system behave as traditionally as
possible while making Objective-C method names case-sensitive.

On the other hand, writing all method names in lower case while
separating parts by hyphens works nicely in all of the `:INVERT`,
`:UPCASE`, `:DOWNCASE`, and `:PRESERVE` modes as well as Allegro CL's
*modern mode*.


## See also:

  __find-selector__, __selector__"

  (typecase selector-name
    (string (intern-selector-by-name selector-name))
    (symbol (intern-selector-by-name (symbol-list->message-name
                                      (list selector-name))))
    (list   (intern-selector-by-name (symbol-list->message-name
                                      selector-name)))))

(defun pointer (thing)
  (etypecase thing
    (c-pointer thing)
    ((or c-pointer-wrapper exception) (pointer-to thing))))

(defun object-is-class-p (obj)
  (%objcl-object-is-class (pointer obj)))

(defun object-is-meta-class-p (obj)
  (%objcl-object-is-meta-class (pointer obj)))

(defun object-get-class (obj)
  (find-objc-class-by-name
   (%objcl-class-name (%objcl-object-get-class (pointer obj)))))

(defun object-get-meta-class (obj)
  (find-objc-meta-class-by-name
   (%objcl-class-name (%objcl-object-get-class (pointer obj)))))

(defun objcl-class-superclass/pointer (class-ptr)
  (let ((superclass-ptr (%objcl-class-superclass class-ptr)))
    (if (and (not (null-pointer-p superclass-ptr))
             (%objcl-object-is-class superclass-ptr))
        (intern-pointer-wrapper t :pointer superclass-ptr)
        nil)))

(defun objcl-class-superclass (class)
  (objcl-class-superclass/pointer (pointer-to class)))

(defun objcl-create-class (class-name superclass
                           protocol-names
                           ivar-names ivar-typespecs)
  (let* ((ivar-typestrings (mapcar #'print-typespec-to-string ivar-typespecs)))
    (with-foreign-string-pool (register-temporary-string
                               allocate-temporary-string)
      (with-foreign-objects ((%protocol-names :pointer (length protocol-names))
                             (%ivar-names :pointer (length ivar-names))
                             (%ivar-typestrings :pointer (length ivar-typestrings)))
        (flet ((fill-foreign-array-from-list (array list type)
                 (loop for element in list
                       for i from 0
                       do (setf (mem-aref array type i) element))))
          (loop for (array . list)
                  in (list (cons %protocol-names protocol-names)
                           (cons %ivar-names ivar-names)
                           (cons %ivar-typestrings ivar-typestrings))
                do (fill-foreign-array-from-list
                        array
                        (mapcar #'allocate-temporary-string
                                list)
                        :pointer)))
        (%objcl-create-class class-name
                             (pointer-to superclass)
                             (length protocol-names)
                             %protocol-names
                             (length ivar-names)
                             %ivar-names
                             %ivar-typestrings)))))

;;; (@* "Low-level Data Conversion")
(eval-when (:compile-toplevel :load-toplevel)
  ;; In order to be able to dispatch over pointer types, we need to
  ;; define an alias of the implementation's own pointer class.  Note
  ;; that this may be T (in GNU CLISP, for example), so it's a good idea
  ;; to use CHECK-TYPE in the method body.
  (unless (find-class 'foreign-pointer nil)
    (setf (find-class 'foreign-pointer nil)
          (class-of (make-pointer 0)))
    (ignore-errors
      (deftype foreign-pointer ()
        '(satisfies cffi:pointerp)))))


(declaim (ftype (function ((or selector string symbol list)) selector)
                selector))
(defun selector (designator)
  "Convert an object into a selector.

## Arguments and Values:

*designator* --- a *selector designator*.


## Description:

*selector-designator* must be a valid *selector designator*, that is:
either a __selector__ object or one of a **symbol**, a **string**, or a
**list** of **symbol**s representing a __selector__.

If *selector-designator* is a **string** or a **list** of **symbol**s,
__intern-selector__ is called and the value returned.

If *selector-designator* is a single **symbol**, it is treated as if it
were a **list** whose **car** is the **symbol** and whose **cdr** is
__nil__.

If *selector-designator* is a __selector__, it is simply returned.


## Examples:

    (selector \"self\")       ;=> #<SELECTOR `self'>
    (selector '(self))      ;=> #<SELECTOR `self'>
    (selector 'self)        ;=> #<SELECTOR `self'>
    (selector *)            ;=> #<SELECTOR `self'>

    (selector 'selph)       ; error

    (selector \"stringWithCString:encoding:\")
      ;=> #<SELECTOR `stringWithCString:encoding:'>

    (selector '(:string-with-c-string :encoding))
      ;=> #<SELECTOR `stringWithCString:encoding:'>

    #.(setq \\*readtable\\* (copy-readtable)) 
    #.(setf (readtable-case \\*readtable\\*) :invert)
    (selector '(:stringWithCString :encoding))
      ;=> #<SELECTOR `stringWithCString:encoding:'>


## Note:

Setting the **readtable case** of the **current readtable** to `:INVERT`
is a good way of making the Lisp system behave as traditionally as
possible while making Objective-C method names case-sensitive.

On the other hand, writing all method names in lower case while
separating parts by hyphens works nicely in all of the `:INVERT`,
`:UPCASE`, `:DOWNCASE`, and `:PRESERVE` modes as well as Allegro CL's
*modern mode*.


## See also:

  __find-selector__, __intern-selector__"

  (etypecase designator
    (selector designator)
    (symbol (selector (list designator)))
    ((or string list)
     (intern-selector designator))))


;;;; (@* "Helper functions")
(defun sizeof (typespec)
  (%objcl-sizeof-type (print-typespec-to-string typespec)))

(defun alignof (typespec)
  (%objcl-alignof-type (print-typespec-to-string typespec)))

(defun return-type-sizeof (typespec)
  (%objcl-sizeof-return-type (print-typespec-to-string typespec)))

(defun runtime-type ()
  (let ((runtime (%objcl-get-runtime-type)))
    (assert (member runtime '("GNU" "NeXT") :test #'string=)
            (runtime)
            "Unkown Objective-C runtime type ~S.  Allowed: (\"GNU\" \"NeXT\")."
            runtime)
    (cond ((string= runtime "GNU") :gnu)
          ((string= runtime "NeXT") :next))))

(defun objc-2.0-runtime-p ()
  (not (zerop (%objcl-objc2-p))))


;;;; (@* "Registry update and maintenance")
(defcallback collect-class-methods :void ((class :pointer))
  (flet ((collect-methods (class)
           (with-foreign-object (count-buf :unsigned-int)
             (let ((method-array (%objcl-class-methods class count-buf)))
               (unwind-protect
                   (dotimes (i (mem-ref count-buf :unsigned-int))
                     (intern-pointer-wrapper 'selector
                                             :pointer
                                             (%objcl-method-selector
                                              (mem-aref method-array :pointer i))))
                 (unless (zerop (mem-ref count-buf :unsigned-int))
                   (foreign-free method-array)))))))
    (collect-methods class)
    (collect-methods (%objcl-class-metaclass class))))

(defun collect-methods ()
  "Intern all method __selector__s known to the runtime.

## Description:

For the syntax enhancement provided by __enable-method-syntax__ to work,
all method __selector__s must be available as the __fdefinition__ of the
**symbol** whose __symbol-name__ is the __selector__'s name and which
are **accessible** in the package _objective-cl-selectors_.
__collect-methods__ ensures that all __selector__s that are known to the
Objective-C runtime as the names of methods of any classes are
registered in this way.

Normally, __collect-methods__ need not be called by user code, as it is
called once at system load-time as well as whenever a framework is
loaded by Objective-CL, but if the user dynamically loads any libraries
or frameworks by calling FFI routines (like, for instance,
_cffi:load-foreign-library_) directly, __collect-methods__ must be
called if the newly introduced __selector__s are to be available to Lisp
code written using the syntax enhancement provided by
__enable-method-syntax__.


## See also:

  __collect-classes__"
  (%objcl-for-each-class-do (callback collect-class-methods)))


;;;; (@* "Slot access")
(defun objcl-slot-type (slot)
  (%objcl-slot-type slot))

(defun objcl-slot-name (slot)
  (%objcl-slot-name slot))

(defun objcl-class-direct-slots (class)
  (if (typep class 'objective-c-class)
      (objcl-class-direct-slots/pointer (pointer-to class))
      nil))

(defun objcl-class-direct-slots/pointer (class-ptr)
  (with-foreign-objects ((count-ptr :unsigned-int)
                         (element-size-ptr :unsigned-int))
    (let ((array-pointer (%objcl-class-direct-slots class-ptr
                                                    count-ptr
                                                    element-size-ptr)))
      (unless (zerop (mem-ref count-ptr :unsigned-int))
        (unwind-protect
            (loop with element-size = (mem-ref element-size-ptr :unsigned-int)
                  with count = (mem-ref count-ptr :unsigned-int)
                  for i from 0 below count
                  for current-slot = array-pointer
                    then (inc-pointer current-slot element-size)
                  collecting (mem-ref current-slot :pointer))
          (foreign-free array-pointer))))))

(defun objcl-class-backed-by-lisp-class-p/pointer (class-ptr)
  (not (zerop (%objcl-class-backed-by-lisp-class-p class-ptr))))

(defun objcl-class-set-backed-by-lisp-class/pointer (class-ptr backed-p)
  (%objcl-class-set-backed-by-lisp-class class-ptr (if backed-p 1 0)))

(defun objcl-object-backed-by-lisp-class-p/pointer (object-ptr)
  (not (zerop (%objcl-object-backed-by-lisp-class-p object-ptr))))

(defun objcl-object-backed-by-lisp-class-p (instance)
  (objcl-object-backed-by-lisp-class-p/pointer (pointer-to instance)))
