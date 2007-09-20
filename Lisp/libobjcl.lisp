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

(defcfun ("objcl_invoke_method"
          %objcl-invoke-method) obj-data
  (receiver obj-data)
  (method-selector :pointer)
  (argc :int)
  &rest)

(defcfun ("objcl_invoke_with_types" %objcl-invoke-with-types) :pointer
  (argc :int)
  (return_typespec :string)
  (arg_typespecs (:array :string))
  (return_value (:pointer :void))
  (argv (:array (:pointer :void))))

(defcfun ("objcl_find_class" %objcl-find-class) :pointer
  (class-name :string))

(defcfun ("objcl_class_name" %objcl-class-name) :string
  (class :pointer))

(defcfun ("objcl_find_selector" %objcl-find-selector) :pointer
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
    (if (cffi:null-pointer-p class-ptr)
        nil
        #-openmcl (make-instance 'objc-class :pointer class-ptr)
        #+openmcl (change-class (make-instance 'c-pointer-wrapper
                                   :pointer value)
                                'objc-class))))


(declaim (ftype (function (string) (or null selector))
                find-selector-by-name))
(defun find-selector-by-name (selector-name)
  (let ((selector-ptr (%objcl-find-selector selector-name)))
    (if (cffi:null-pointer-p selector-ptr)
        nil
        (make-instance 'selector :pointer selector-ptr))))


(declaim (ftype (function ((or objc-class id exception)) string)
                objcl-class-name))
(defun objcl-class-name (class)
  (declare (type (or objc-class id exception) class))
  (%objcl-class-name (pointer-to class)))


(declaim (ftype (function (selector) string) selector-name))
(defun selector-name (selector)
  (declare (type selector selector))
  (%objcl-selector-name (pointer-to selector)))


(declaim (ftype (function ((or id objc-class exception) selector) *)
                get-method-implementation))
(defun get-method-implementation (object selector)
  (declare (type selector selector))
  (%objcl-get-method-implementation (pointer-to object)
                                    (pointer-to selector)))


(declaim (ftype (function ((or selector string list)) (or null selector))
                find-selector))
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


(defun object-is-class-p (obj)
  (%objcl-object-is-class (pointer-to obj)))

(defun object-is-meta-class-p (obj)
  (%objcl-object-is-meta-class (pointer-to obj)))

(defun object-get-class (obj)
  (make-instance 'objc-class
     :pointer (%objcl-object-get-class (pointer-to obj))))

(defun object-get-meta-class (obj)
  (make-instance 'objc-meta-class
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
__find-selector__ is called and the value returned, except that if
__find-selector__ returns __nil__, an **error** is **signal**ed.

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
     (or (find-selector designator)
         (error "Could not find the selector designated by ~S."
                designator)))))


(declaim (ftype (function (*)
                          (values foreign-pointer &rest nil))
                lisp->obj-data))
(defun lisp->obj-data (value)
  (let ((obj-data (foreign-alloc 'obj-data))
        (type-name (lisp-value->type-name value)))
    (with-foreign-slots ((type data) obj-data obj-data)
      (setf (foreign-slot-value data
                                'obj-data-union
                                (type-name->slot-name type-name))
            (typecase value
              (symbol (selector value))
              ((or id objc-class selector exception)
               (pointer-to value))
              (string (foreign-string-alloc value))
              (otherwise value)))
      (setf type
            (foreign-string-alloc (type-name->type-id type-name))))
    obj-data))


(declaim (ftype (function (foreign-pointer)
                          (values (or number string symbol selector id
                                      objc-class boolean foreign-pointer)
                                  &rest nil))
                obj-data->lisp))
(defun obj-data->lisp (obj-data)
  (with-foreign-slots ((type data) obj-data obj-data)
    (let* ((type-name (type-id->type-name (if (stringp type)
                                              type
                                              (foreign-string-to-lisp type))))
           (lisp-type (type-name->lisp-type type-name))
           (value     (if (eq 'void type-name)
                          (values)
                          (foreign-slot-value data
                                              'obj-data-union
                                              (type-name->slot-name type-name)))))
      (case lisp-type
        ((id objc-class selector exception)
         (make-instance lisp-type :pointer value) )
        ((string) (foreign-string-to-lisp value))
        (otherwise value)))))


(declaim (ftype (function (foreign-pointer) (values string &rest nil))
                foreign-string-to-lisp/dealloc))
(defun foreign-string-to-lisp/dealloc (foreign-string)
  "Convert a (possibly freshly allocated) C string into a Lisp string
and free the C string afterwards."

  (unwind-protect
       (foreign-string-to-lisp foreign-string)
    (foreign-string-free foreign-string)))


(defun parse-typespec (typestring &optional (start 0))
  "Parse a typestring like \"@0:4{_NSRange=II}8\" into something like (ID ()).

\"rn{_NSRange=II}8\" is parsed into (STRUCT (CONST IN)
\"_NSRange\" :INTEGER :INTEGER).

Returns: (VALUES typespec byte-position string-position)"

  (let ((init-char (char typestring start))
        (string-position start)
        (qualifiers (list)))
    (loop do (setq init-char (char typestring string-position))
          while (let ((qualifier (case init-char
                                   (#\r 'const)
                                   (#\n 'in)
                                   (#\N 'inout)
                                   (#\o 'out)
                                   (#\O 'bycopy)
                                   (#\V 'oneway)
                                   (#\R 'byref))))
                  (and qualifier
                       (incf string-position)
                       (push qualifier qualifiers))))
    (values (case init-char
              ((#\{ #\()
               (let* ((=-token (position #\= typestring :start start))
                      (name-end (or =-token
                                    ;; An opaque struct whose contents
                                    ;; we don't know.
                                    (position (ecase init-char
                                               (#\{ #\})
                                               (#\( #\)))
                                              typestring
                                              :start start)
                                    (error "Premature end of file in~
                                            typespec: ~A."
                                           typestring)))
                      (struct-name (subseq typestring
                                           (1+ string-position)
                                           name-end)))
                 (list* (ecase init-char
                          (#\{ 'struct)
                          (#\( 'union))
                        (if =-token
                            qualifiers
                            (cons 'opaque qualifiers))
                        struct-name
                        (progn
                          (setq string-position
                                (if =-token
                                    (1+ name-end) ; skip #\=
                                    name-end))
                          (loop until (char= (char typestring string-position)
                                             (ecase init-char
                                               (#\{ #\})
                                               (#\( #\))))
                                collect (multiple-value-bind (typespec
                                                              byte-position
                                                              new-string-pos)
                                            (parse-typespec
                                             typestring
                                             string-position)
                                          (declare (ignore byte-position))
                                          (setq string-position new-string-pos)
                                          typespec)
                                ;; Skip end marker (right brace/paren).
                                finally (incf string-position))))))
              (#\^ (list 'pointer
                         qualifiers
                         (multiple-value-bind (typespec byte-pos new-str-pos)
                             (parse-typespec typestring (1+ string-position))
                           (declare (ignore byte-pos))
                           (prog1 typespec
                             (setq string-position new-str-pos)))))
              (#\[ (list 'array
                         qualifiers
                         (multiple-value-bind (count new-str-pos)
                             (parse-integer typestring
                                            :start (1+ string-position)
                                            :junk-allowed t)
                           (prog1 count
                             (setq string-position new-str-pos)))
                         (multiple-value-bind (typespec byte-pos new-str-pos)
                             (parse-typespec typestring string-position)
                           (declare (ignore byte-pos))
                           ;; Skip end marker (right bracket).
                           (prog1 typespec
                             (setq string-position (1+ new-str-pos))))))
              (#\j
               (list 'complex
                     qualifiers
                     (multiple-value-bind (typespec byte-pos new-str-pos)
                         (parse-typespec typestring (1+ string-position))
                       (declare (ignore byte-pos))
                       (prog1 typespec
                         (setq string-position new-str-pos)))))
              (#\b
               (let (bit-field-starting-pos
                     bit-field-typespec
                     bit-field-length
                     byte-position)
                 (multiple-value-setq (bit-field-starting-pos string-position)
                     (parse-integer typestring
                                    :start (1+ string-position)
                                    :junk-allowed t))
                 (multiple-value-setq (bit-field-typespec
                                       byte-position
                                       string-position)
                     (parse-typespec typestring string-position))
                 (multiple-value-setq (bit-field-length string-position)
                     (parse-integer typestring
                                    :start string-position
                                    :junk-allowed t))
                 (list 'bit-field
                       qualifiers
                       bit-field-starting-pos
                       bit-field-length
                       bit-field-typespec)))
              (otherwise
               (prog1 (list (case init-char
                              (#\B :boolean)
                              (#\c :char)
                              (#\C :unsigned-char)
                              (#\s :short)
                              (#\S :unsigned-short)
                              (#\i :int)
                              (#\I :unsigned-int)
                              (#\l :long)
                              (#\L :unsigned-long)
                              (#\q :long-long)
                              (#\Q :unsigned-long-long)
                              (#\f :float)
                              (#\d :double)
                              (#\v :void)
                              (#\@ 'id)
                              (#\# 'objc-class)
                              (#\: 'selector)
                              (#\* :string)
                              (#\? :unknown))
                            qualifiers)
                 (incf string-position))))
            #+(or)  ; too greedy (=> bit-fields can't see their length!)
            (multiple-value-bind (byte-position new-string-pos)
                (parse-integer typestring
                               :start string-position
                               :junk-allowed t)
              (setq string-position new-string-pos)
              byte-position)
            #-(or) nil
            string-position)))
