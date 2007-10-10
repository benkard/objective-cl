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


(dolist (subdir '("shared_obj" "obj"))
  (pushnew
   (merge-pathnames (make-pathname :directory (list
                                               :relative "Objective-C" subdir)
                                   :type ""
                                   :name "")
                    (asdf:component-pathname (asdf:find-system
                                              '#:objective-cl)))
   cffi:*foreign-library-directories*))

(define-foreign-library libobjcl
  (:unix (:or "libobjcl.so"
              "libobjcl.so.0"
              "libobjcl.dylib"
              "libobjcl.dylib.0"))
  (t (:default "libobjcl")))

(use-foreign-library libobjcl)


(defcfun ("objcl_initialise_runtime" %initialise-runtime) :void)

(defcfun ("objcl_shutdown_runtime" %shutdown-runtime) :void)

(defcfun ("objcl_invoke_with_types" %objcl-invoke-with-types) :pointer
  (argc :int)
  (return_typespec :string)
  (arg_typespecs (:array :string))
  (return_value (:pointer :void))
  (argv (:array (:pointer :void))))

(defcfun ("objcl_find_class" %objcl-find-class) :pointer
  (class-name :string))

(defcfun ("objcl_find_meta_class" %objcl-find-meta-class) :pointer
  (class-name :string))

(defcfun ("objcl_class_name" %objcl-class-name) :string
  (class :pointer))

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

(defcfun ("objcl_sizeof_type" %objcl-sizeof-type) :long
  (typespec :string))

(defcfun ("objcl_sizeof_return_type" %objcl-sizeof-return-type) :long
  (typespec :string))

(defcfun ("objcl_alignof_type" %objcl-alignof-type) :long
  (typespec :string))

(defcfun objcl-get-nil :pointer)
(defcfun objcl-get-yes :long)
(defcfun objcl-get-no :long)

(defun initialise-runtime ()
  "Initialise the Objective C runtime.

## Description:

The function __initialise-runtime__ makes all the necessary arrangements
for object instantiation and method calls to work.  In particular, it
creates an autorelease pool in order to make the use of Objective C's
semiautomatic reference counting memory management possible, which is
used internally by Objective CL.

Note that, as the autorelease pool created by __initialise-runtime__ is
currently only deallocated and its containees released when
__shutdown-runtime__ is called, it is generally advisable to make use of
AppKit's automatic creation and deletion auf autorelease pools, if
possible.  Naturally, AppKit-based applications need not worry about
this, but be aware that they do need to call __initialise-runtime__
before making any other Objective C calls.


## See also:

  __shutdown-runtime__"

  (when (zerop *runtime-initialisation-level*)
    (%initialise-runtime))
  (atomically (incf *runtime-initialisation-level*)))


(defun shutdown-runtime ()
  "Shut the Objective C runtime down.

## Description:

The function __shutdown-runtime__ cleans the environment up.  In
particular, it tries to release all the objects retained in any
autorelease pools created by __initialise-runtime__.

Note that even if you make use of AppKit, which manages its own
autorelease pools, you must call __initialise-runtime__ before making
any Objective C calls, and you should call __shutdown-runtime__ when you
are finished with Objective C, since Objective CL makes use of
autoreleased objects internally before you are even able to retrieve any
objects or classes, let alone send messages to them.


## See also:

  __initialise-runtime__"

  (when (zerop (atomically (decf *runtime-initialisation-level*)))
    (%shutdown-runtime)))


(declaim (ftype (function ((or string symbol) &optional t)
                          (or null objc-class))
                find-objc-class))
(defun find-objc-class (class-name &optional errorp)
  "Retrieve an Objective C class by name.

## Arguments and Values:

*class-name* --- a **symbol** or a **string**.

*errorp* --- a **generalized boolean**.

Returns: *class* --- an __objc-class__ object representing the Objective
C class whose name is *class-name*.


## Description:

If no Objective C class named by *class-name* is found, the behaviour
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

The first component of an Objective C class name is conventionally
thought of as a namespace identifier.  It is therefore sensible to
expect it to be converted to **uppercase** by default, which is the
conventional case for namespace identifiers in Objective C."

  (let ((class
         (etypecase class-name
           (string (find-objc-class-by-name class-name))
           (symbol (find-objc-class-by-name
                    (symbol->objc-class-name class-name))))))
    (or class (if errorp
                  (error "Found no Objective C class named ~S."
                         class-name)
                  nil))))


(declaim (ftype (function (string) (or null objc-class))
                find-objc-class-by-name))
(defun find-objc-class-by-name (class-name)
  (let ((class-ptr (%objcl-find-class class-name)))
    (if (objc-pointer-null class-ptr)
        nil
        #-(or t openmcl) (make-pointer-wrapper 'objc-class :pointer class-ptr)
        #+(and nil openmcl) (change-class (make-pointer-wrapper 'c-pointer-wrapper
                                             :pointer value)
                                          'objc-class))))


(defun find-objc-meta-class (meta-class-name &optional errorp)
  (let ((meta-class
         (etypecase meta-class-name
           (string (find-objc-meta-class-by-name meta-class-name))
           (symbol (find-objc-meta-class-by-name
                    (symbol->objc-class-name meta-class-name))))))
    (or meta-class (if errorp
                       (error "Found no Objective C metaclass named ~S."
                              meta-class-name)
                       nil))))


(defun find-objc-meta-class-by-name (class-name)
  (let ((class-ptr (%objcl-find-meta-class class-name)))
    (if (objc-pointer-null class-ptr)
        nil
        #-(or t openmcl) (make-pointer-wrapper 'objc-meta-class :pointer class-ptr)
        #+(and nil openmcl) (change-class (make-pointer-wrapper 'c-pointer-wrapper
                                             :pointer value)
                                          'objc-meta-class))))

(defun objc-pointer-null (pointer)
  (or (cffi:null-pointer-p pointer)
      (cffi:pointer-eq pointer (objcl-get-nil))))


(declaim (ftype (function (string) (or null selector))
                find-selector-by-name))
(defun find-selector-by-name (selector-name)
  (let ((selector-ptr (%objcl-find-selector selector-name)))
    (if (cffi:null-pointer-p selector-ptr)
        nil
        (make-pointer-wrapper 'selector :pointer selector-ptr))))


(defun intern-selector-by-name (selector-name)
  (let ((selector-ptr (%objcl-intern-selector selector-name)))
    (assert (not (cffi:null-pointer-p selector-ptr))
            (selector-ptr)
            "%OBJCL-INTERN-SELECTOR must always return a selector.")
    (make-pointer-wrapper 'selector :pointer selector-ptr)))


(declaim (ftype (function ((or objc-class id exception)) string)
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
  (declare (type (or objc-class id exception) class))
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


(declaim (ftype (function ((or id objc-class exception) selector) t)
                get-method-implementation))
(defun get-method-implementation (object selector)
  (declare (type selector selector))
  (%objcl-get-method-implementation (pointer-to object)
                                    (pointer-to selector)))


(declaim (ftype (function ((or selector symbol string list) &optional t)
                          (or null selector))
                find-selector))
(defun find-selector (selector-name &optional errorp)
  "Retrieve a method selector by name.

## Arguments and Values:

*selector-name* --- a **string**, a **symbol**, or a **list** of **symbol**s.

*errorp* --- a **generalized boolean**.

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

1. The first part is fully converted to **lowercase**.

2. Any additional parts are also fully converted to **lowercase** except
   for their first letters, which are left intact.

3. If the symbol is a **keyword**, the resulting **string** is suffixed
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


## See also:

  __intern-selector__"

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

1. The first part is fully converted to **lowercase**.

2. Any additional parts are also fully converted to **lowercase** except
   for their first letters, which are left intact.

3. If the symbol is a **keyword**, the resulting **string** is suffixed
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


## See also:

  __find-selector__"

  (typecase selector-name
    (string (intern-selector-by-name selector-name))
    (symbol (intern-selector-by-name (symbol-list->message-name
                                      (list selector-name))))
    (list   (intern-selector-by-name (symbol-list->message-name
                                      selector-name)))))


(defun object-is-class-p (obj)
  (%objcl-object-is-class (pointer-to obj)))

(defun object-is-meta-class-p (obj)
  (%objcl-object-is-meta-class (pointer-to obj)))

(defun object-get-class (obj)
  (make-pointer-wrapper 'objc-class
     :pointer (%objcl-object-get-class (pointer-to obj))))

(defun object-get-meta-class (obj)
  (make-pointer-wrapper 'objc-meta-class
     :pointer (%objcl-object-get-meta-class (pointer-to obj))
     :meta-class-for-class (object-get-class obj)))

(defun objc-class-of (obj)
  (cond ((object-is-meta-class-p obj)
         (error "Tried to get the class of meta class ~S." obj))
        ((object-is-class-p obj) (object-get-meta-class obj))
        (t (object-get-class obj))))


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
      ;=> #<SELECTOR `stringWithCString:encoding:'>"

  (etypecase designator
    (selector designator)
    (symbol (selector (list designator)))
    ((or string list)
     (intern-selector designator))))


;;;; (@* "Helper functions")
(defun sizeof (typespec)
  (%objcl-sizeof-type typespec))

(defun alignof (typespec)
  (%objcl-alignof-type typespec))

(defun return-type-sizeof (typespec)
  (%objcl-sizeof-return-type typespec))

(defun runtime-type ()
  (let ((runtime (%objcl-get-runtime-type)))
    (assert (member runtime '("GNU" "NeXT") :test #'string=)
            (runtime)
            "Unkown Objective-C runtime type ~S.  Allowed: (\"GNU\" \"NeXT\")."
            runtime)
    (cond ((string= runtime "GNU") :gnu)
          ((string= runtime "NeXT") :next))))
