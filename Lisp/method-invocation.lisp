(in-package #:mulk.objective-cl)


;;; (@* "Method invocation")
(defun invoke (receiver message-start &rest message-components)
  "Send a message to an Objective C instance.

## Arguments and Values:

*receiver* --- an Objective C wrapper object.

*message-start* --- a **symbol**.

*message-components* --- an alternating **list** of **object**s and
**symbol**s.

Returns: *result* --- the return value of the method invocation.


## Description:

All even-numbered *message components* are collected in order and the
resulting **list** used as if as additional **argument**s to
__invoke-by-name__.

All uneven-numbered *message components*, which must be **symbol**s, are
first split into parts separated by hyphens and each part converted into
a **string** according to the following rules:

1. The first part is fully converted to **lowercase**.

2. Any additional parts are also fully converted to **lowercase** except
   for their first letters, which are left intact.

3. If the symbol is a **keyword**, the resulting **string** is suffixed
   by a **colon** (`:').

After that, all parts are concatenated in order to form a
single *message name component*.  The *message name components* are in
turn concatenated in order to form the *message name* which is used as
if as the second **argument** to __invoke-by-name__.


## Examples:

    (invoke (find-objc-class 'ns-string)
            :string-with-c-string \"Mulk.\")
      ;=> #<GSCBufferString `Mulk.' {5B36087}>

    (invoke (find-objc-class 'ns-object)
            'self)                           
      ;=> #<NSObject `NSObject' {16ECF598}>

    (invoke (find-objc-class 'ns-object)
            'name)                           
      ;=> \"NSObject\"

    (invoke (find-objc-class 'ns-string)
            :string-with-c-string \"Mulk.\"
            :encoding 4)
      ;=> #<GSCBufferString `Mulk.' {5B36087}>


## See also:

  __invoke-by-name__"

  (check-type receiver (or id objc-class exception)
              "an Objective C instance (ID, OBJC-CLASS or EXCEPTION)")
  (do* ((components-left (cons message-start message-components)
                         (cddr components-left))
        (message-list    (list message-start)
                         (cons (first components-left) message-list))
        (arglist         (if (null (rest components-left))
                             nil
                             (list (second components-left)))
                         (if (null (rest components-left))
                             arglist
                             (cons (second components-left) arglist))))
       ((null (cddr components-left))
        (apply #'invoke-by-name
               receiver
               (symbol-list->message-name (nreverse message-list))
               (nreverse arglist)))))


(defun invoke-by-name (receiver method-name &rest args)
  "Send a message to an Objective C object by the name of the method.

## Arguments and Values:

*receiver* --- an Objective C wrapper object.

*method-name* --- a **string**.

*args* --- a list of **object**s.

Returns: *result* --- the return value of the method invocation.


## Examples:

    (invoke-by-name (find-objc-class 'ns-string)
                    \"stringWithCString:\" \"Mulk.\")
      ;=> #<GSCBufferString `Mulk.' {5B36087}>

    (invoke-by-name (find-objc-class 'ns-object)
                    \"self\")
      ;=> #<NSObject `NSObject' {16ECF598}>

    (invoke-by-name (find-objc-class 'ns-string)
                    \"stringWithCString:encoding:\"
                    \"Mulk.\"
                    4)
      ;=> #<GSCBufferString `Mulk.' {5B36087}>


## See also:

  __invoke__"

  (check-type receiver (or id objc-class exception)
              "an Objective C instance (ID, OBJC-CLASS or EXCEPTION)")
  (when *trace-method-calls*
    (format t "~&Invoking [~A].~%" method-name))
  (flet ((convert/signal (foreign-value)
           ;; Convert a foreign value into a Lisp value.  If the value
           ;; to be converted represents an exception, signal it instead
           ;; of returning it as a value.
           (let ((lisp-value (obj-data->lisp foreign-value)))
             (if (typep lisp-value 'condition)
                 (cerror "Return NIL from OBJCL-INVOKE-METHOD." lisp-value)
                 lisp-value))))
    (let ((objc-arglist (arglist->objc-arglist args))
          (selector (selector method-name)))
      (unwind-protect
           (with-foreign-conversion ((objc-receiver receiver))
             (with-foreign-objects ((return-value
                                     (apply-macro '%objcl-invoke-method
                                                  objc-receiver
                                                  (pointer-to selector)
                                                  (length args)
                                                  objc-arglist)))
               (let ((*skip-retaining* (or *skip-retaining*
                                           (constructor-name-p method-name))))
                 (convert/signal return-value))))
        (dealloc-objc-arglist objc-arglist)))))


;;; (@* "Helper functions")
(defun arglist->objc-arglist (arglist)
  (arglist-intersperse-types (mapcar #'lisp->obj-data arglist)))


(defun dealloc-objc-arglist (objc-arglist)
  (do ((objc-arglist objc-arglist (cddr objc-arglist)))
      ((null objc-arglist))
    ;; (first objc-arglist) is a CFFI type name.
    (dealloc-obj-data (second objc-arglist))))


(defun arglist-intersperse-types (arglist)
  (mapcan #'(lambda (arg)
              (list :pointer arg))
          arglist))


(defun constructor-name-p (method-name)
  (flet ((method-name-starts-with (prefix)
           (and (>= (length method-name) (length prefix))
                (or (and (string= prefix
                                  (subseq method-name 0 (length prefix)))
                         (or (= (length method-name)
                                (length prefix))
                             (not (lower-case-p (char method-name (length prefix))))))))))
    (or (method-name-starts-with "alloc")
        (method-name-starts-with "new"))))


;;; (@* "High-level Data Conversion")
(defgeneric ->id (x))
(defgeneric ->class (x))
(defgeneric ->integer (x))
(defgeneric ->selector (x))
(defgeneric ->exception (x))
(defgeneric ->character (x))
(defgeneric ->float (x))
(defgeneric ->double (x))
(defgeneric ->bool (x))
(defgeneric ->string (x))
(defgeneric ->pointer (x))


(defmethod ->id ((x id))
  x)

(defmethod ->id ((x class))
  (invoke x 'self))

(defmethod ->id ((x exception))
  (invoke x 'self))

(defmethod ->id ((x integer))
  (let ((id (invoke (find-class 'ns-number)
                    :number-with-long x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))

(defmethod ->id ((x float))
  (let ((id (invoke (find-class 'ns-number)
                    :number-with-double x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))

(defmethod ->id ((x string))
  (let ((id (invoke (find-class 'ns-string)
                    :string-with-c-string x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))


(defmethod ->class ((x id))
  (invoke x 'class))

(defmethod ->class ((x exception))
  (invoke x 'class))

(defmethod ->class ((x class))
  x)

(defmethod ->class ((x string))
  (find-objc-class x t))

(defmethod ->class ((x symbol))
  (find-objc-class x t))


(defmethod ->integer ((x id))
  (assert (objc-typep x 'ns-number))
  (invoke x 'long-value))

(defmethod ->integer ((x number))
  (truncate x))

(defmethod ->integer ((x null))
  0)

(defmethod ->integer ((x symbol))
  (assert (eq 't x))
  1)


(defmethod ->selector ((x selector))
  x)

(defmethod ->selector ((x symbol))
  (selector x))

(defmethod ->selector ((x string))
  (selector x))

(defmethod ->selector ((x cons))
  (selector x))


(defmethod ->exception ((x exception))
  x)


(defmethod ->character ((x character))
  x)

(defmethod ->character ((x integer))
  x)


(defmethod ->float ((x number))
  (float x))


(defmethod ->double ((x number))
  (float x))


(defmethod ->bool ((x null))
  x)

(defmethod ->bool ((x symbol))
  (assert (eq 't x))
  x)

(defmethod ->bool ((x integer))
  x)


(defmethod ->string ((x string))
  x)

(defmethod ->string ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)


(defmethod ->pointer ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)

(defmethod ->pointer ((x exception))
  (pointer-to x))

(defmethod ->pointer ((x c-pointer-wrapper))
  (pointer-to x))

(defmethod ->pointer ((x number))
  (pointer-to (->id x)))
