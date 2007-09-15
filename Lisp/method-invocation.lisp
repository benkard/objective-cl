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
             (with-obj-data-values ((return-value
                                     (apply-macro '%objcl-invoke-method
                                                  objc-receiver
                                                  (pointer-to selector)
                                                  (length args)
                                                  objc-arglist)))
               (let ((*skip-retaining* (or *skip-retaining*
                                           (constructor-name-p method-name))))
                 (convert/signal return-value))))
        (dealloc-objc-arglist objc-arglist)))))


(defmacro unsafe-primitive-invoke (receiver method-name return-type &rest args)
  (let ((real-return-type (if (member return-type '(id class exception))
                              :pointer
                              return-type))
        (real-receiver (gensym))
        (real-selector (gensym))
        (selector (selector method-name)))
    `(progn
       (let ((,real-receiver ,receiver)
             (,real-selector (selector ,method-name)))
         (check-type ,real-receiver (or id objc-class exception)
                     "an Objective C instance (ID, OBJC-CLASS or EXCEPTION)")
         (let ((method (get-method-implementation ,real-receiver ,real-selector))
               (objc-arglist (arglist->objc-arglist (list ,@args))))
           (unwind-protect
               (let ((return-value
                      (apply-macro 'foreign-funcall-pointer method
                                   ()
                                   (append (list :pointer (pointer-to ,real-receiver))
                                           (list :pointer (pointer-to ,real-selector))
                                           objc-arglist
                                           (list ,real-return-type)))))
                 ,(if (member return-type '(id class exception))
                      `(let (,@(when (constructor-name-p (selector-name selector))
                                 `((*skip-retaining* t))))
                         (make-instance ',(case return-type
                                            ((id) 'id)
                                            ((class) 'objc-class)
                                            ((exception) 'exception))
                            :pointer return-value))
                      `return-value))
             (dealloc-objc-arglist objc-arglist)))))))


(defun primitive-invoke (receiver method-name return-type &rest args)
  (let ((return-c-type (case return-type
                         ((id class exception selector) :pointer)
                         (otherwise return-type)))
        (return-type-cell (cffi:foreign-string-alloc
                           (type-name->type-id return-type)))
        (selector (selector method-name)))
    (cffi:with-foreign-objects ((arg-types '(:pointer :char) (length args))
                                (objc-args '(:pointer :void) (+ (length args) 2))
                                (return-value-cell return-c-type))
      (flet ((ad-hoc-arglist->objc-arglist! (args)
               (setf (cffi:mem-aref objc-args '(:pointer :void) 0)
                     (pointer-to receiver)
                     (cffi:mem-aref objc-args '(:pointer :void) 1)
                     (pointer-to selector))
               (loop for arg in args
                     for i from 2
                     do (let* ((type-name (lisp-value->type-name arg))
                               #+(or)
                               (cffi-type (type-name->lisp-type type-name)))
                          (setf (cffi:mem-aref objc-args '(:pointer :void) i)
                                (typecase arg
                                  #+(or)
                                  (c-pointer
                                   ;; Assume that arg points to a struct,
                                   ;; and that the method wants a copy of
                                   ;; that struct, not the pointer itself.
                                   arg)
                                  (t (cffi:foreign-alloc
                                      #+(or) cffi-type
                                      :pointer
                                      :initial-element (typecase arg
                                                         (c-pointer-wrapper
                                                          (print (pointer-to arg)))
                                                         (t arg))))))
                          (setf (cffi:mem-aref arg-types '(:pointer :char) i)
                                (cffi:foreign-string-alloc
                                 (typecase arg
                                   #+(or) (c-pointer "{?=}")
                                   (t (type-name->type-id type-name))))))))
             (dealloc-ad-hoc-objc-arglist ()
               (dotimes (i (length args))
                 (cffi:foreign-free
                  (cffi:mem-aref objc-args '(:pointer :void) (+ i 2)))
                 (cffi:foreign-string-free
                  (cffi:mem-aref arg-types '(:pointer :char) i)))))
        (ad-hoc-arglist->objc-arglist! args)
        (unwind-protect
            (let ((error-cell
                   (%objcl-invoke-with-types (length args)
                                             return-type-cell
                                             arg-types
                                             return-value-cell
                                             objc-args)))
              (unless (cffi:null-pointer-p error-cell)
                (error (make-instance 'exception :pointer error-cell)))
              (case return-type
                ((id class exception selector)
                 (let ((*skip-retaining*
                        (or *skip-retaining*
                            (constructor-name-p (selector-name selector)))))
                   (make-instance return-type
                      :pointer (cffi:mem-ref return-value-cell
                                             return-c-type))))
                (otherwise (cffi:mem-ref return-value-cell
                                         return-c-type))))
          (dealloc-ad-hoc-objc-arglist)
          (foreign-string-free return-type-cell))))))


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
(defgeneric coerce-object (object type))

(defcoercion id ((x id))
  x)

(defcoercion id ((x class))
  (invoke x 'self))

(defcoercion id ((x exception))
  (invoke x 'self))

(defcoercion id ((x integer))
  (let ((id (invoke (find-class 'ns-number)
                    :number-with-long x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))

(defcoercion id ((x float))
  (let ((id (invoke (find-class 'ns-number)
                    :number-with-double x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))

(defcoercion id ((x string))
  (let ((id (invoke (find-class 'ns-string)
                    :string-with-c-string x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))


(defcoercion class ((x id))
  (invoke x 'class))

(defcoercion class ((x exception))
  (invoke x 'class))

(defcoercion class ((x class))
  x)

(defcoercion class ((x string))
  (find-objc-class x t))

(defcoercion class ((x symbol))
  (find-objc-class x t))


(defcoercion integer ((x id))
  (assert (objc-typep x 'ns-number))
  (invoke x 'long-value))

(defcoercion integer ((x number))
  (truncate x))

(defcoercion integer ((x null))
  0)

(defcoercion integer ((x symbol))
  (assert (eq 't x))
  1)


(defcoercion selector ((x selector))
  x)

(defcoercion selector ((x symbol))
  (selector x))

(defcoercion selector ((x string))
  (selector x))

(defcoercion selector ((x cons))
  (selector x))


(defcoercion exception ((x exception))
  x)


(defcoercion character ((x character))
  x)

(defcoercion character ((x integer))
  x)


(defcoercion float ((x number))
  (float x))


(defcoercion double ((x number))
  (float x))


(defcoercion bool ((x null))
  x)

(defcoercion bool ((x symbol))
  (assert (eq 't x))
  x)

(defcoercion bool ((x integer))
  x)


(defcoercion string ((x string))
  x)

(defcoercion string ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)


(defcoercion pointer ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)

(defcoercion pointer ((x exception))
  (pointer-to x))

(defcoercion pointer ((x c-pointer-wrapper))
  (pointer-to x))

(defcoercion pointer ((x number))
  (pointer-to (coerce-object x 'id)))
