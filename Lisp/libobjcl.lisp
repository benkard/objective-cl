(in-package #:mulk.objective-cl)


(pushnew
  (merge-pathnames (make-pathname :directory '(:relative "Objective-C"
                                                         "shared_obj")
                                  :type ""
                                  :name "")
                   (asdf:component-pathname (asdf:find-system
                                             '#:objective-cl)))
   cffi:*foreign-library-directories*)

(define-foreign-library libobjcl
  (:unix (:or "libobjcl.so"
              "libobjcl.so.0"))
  (t (:default "libobjcl")))

(use-foreign-library libobjcl)


(defcfun ("objcl_initialise_runtime" initialise-runtime) :void)
(setf (documentation #'initialise-runtime 'function)
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

  __shutdown-runtime__")


(defcfun ("objcl_shutdown_runtime" shutdown-runtime) :void)
(setf (documentation #'shutdown-runtime 'function)
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

  __initialise-runtime__")


(defcfun ("objcl_invoke_instance_method"
          %objcl-invoke-instance-method) obj-data
  (receiver obj-data)
  (method-name :string)
  (argc :int)
  &rest)

(defcfun ("objcl_invoke_class_method"
          %objcl-invoke-class-method) obj-data
  (receiver obj-data)
  (method-name :string)
  (argc :int)
  &rest)

(defcfun ("objcl_find_class" %objcl-find-class) :pointer
  (class-name :string))

(defcfun ("objcl_class_name" %objcl-class-name) :pointer
  (class obj-data))

(defcfun ("objcl_find_selector" %objcl-find-selector) :pointer
  (selector-name :string))

(defcfun ("objcl_selector_name" %objcl-selector-name) :pointer
  (selector obj-data))


(defun find-objc-class (class-name)
  "Retrieve an Objective C class by name.

## Arguments and Values:

*class-name* --- a **symbol** or a **string**.

Returns: *class* --- an __objc-class__ object representing the Objective
C class whose name is *class-name*.


## Description:

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

  (typecase class-name
    (string (find-objc-class-by-name class-name))
    (symbol (find-objc-class-by-name (symbol->objc-class-name class-name)))))


(defun find-objc-class-by-name (class-name)
  (with-foreign-objects ((obj-data (%objcl-find-class class-name)))
    (if (null-pointer-p (foreign-slot-value
                         (foreign-slot-value obj-data 'obj-data 'data)
                         'obj-data-union
                         'class-val))
        nil
        (obj-data->lisp obj-data))))


(defun find-selector-by-name (selector-name)
  (with-foreign-objects ((obj-data (%objcl-find-selector selector-name)))
    (if (null-pointer-p (foreign-slot-value
                         (foreign-slot-value obj-data 'obj-data 'data)
                         'obj-data-union
                         'sel-val))
        nil
        (obj-data->lisp obj-data))))


(defun objcl-class-name (class)
  (declare (type (or objc-class id exception) class))
  (with-foreign-conversion ((obj-data class))
    (foreign-string-to-lisp/dealloc (%objcl-class-name obj-data))))


(defun selector-name (selector)
  (declare (type selector selector))
  (with-foreign-conversion ((obj-data selector))
    (foreign-string-to-lisp/dealloc (%objcl-selector-name obj-data))))


(defun find-selector (selector-name)
  "Retrieve a method selector by name.

## Arguments and Values:

*selector-name* --- a **string** or a **list** of **symbol**s.

Returns: *selector* --- a __selector__ object, or __nil__.


## Description:

If *selector-name* is a **string**, the __selector__ named by that
string is returned.  If no __selector__ with the given name exists,
__nil__ is returned.

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

    (find-selector \"stringWithCString:encoding:\")
      ;=> #<SELECTOR `stringWithCString:encoding:'>

    (find-selector '(:string-with-c-string :encoding))
      ;=> #<SELECTOR `stringWithCString:encoding:'>"

  (typecase selector-name
    (string (find-selector-by-name selector-name))
    (list   (find-selector-by-name (symbol-list->message-name
                                    selector-name)))))
