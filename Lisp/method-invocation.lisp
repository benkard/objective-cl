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
  (let ((real-return-type (if (member return-type '(id objc-class exception))
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
                 ,(if (member return-type '(id objc-class exception))
                      `(let (,@(when (constructor-name-p (selector-name selector))
                                 `((*skip-retaining* t))))
                         (make-instance ',(case return-type
                                            ((id) 'id)
                                            ((objc-class) 'objc-class)
                                            ((exception) 'exception))
                            :pointer return-value))
                      `return-value))
             (dealloc-objc-arglist objc-arglist)))))))


(defun primitive-invoke (receiver method-name return-type &rest args)
  "An invocation mechanism with ad-hoc argument conversion."
  (with-foreign-string-pool (register-temporary-string)
    (let* ((raw-argc (the argument-number (length args)))
           (real-argc (+ raw-argc 2))
           (return-c-type (case return-type
                            ((id objc-class exception selector) :pointer)
                            (otherwise return-type)))
           (selector (selector method-name)))
      (labels ((alloc-string-and-register (string)
                 (register-temporary-string
                  (cffi:foreign-string-alloc string))))
        ;; We allocate a conservatively-sized buffer for arguments of
        ;; primitive types called OBJC-ARG-BUFFER.  Non-primitive types
        ;; don't need allocation, anyway, because we can just pass the
        ;; pointer directly.  It's unfortunate that we can't do this for
        ;; `id' values, because we can't just pass a pointer to the `id'
        ;; SAP (which would be highly implementation-dependent and might
        ;; even change at any time, especially during GC).
        ;;
        ;; In any case, OBJC-ARGS-PTRS is the array of pointers which
        ;; the libffi docs call AVALUES.  It must therefore contain
        ;; pointers pointing into the argument buffer (or, in the case
        ;; of a newly allocated C string, to that string).  This is what
        ;; the DOTIMES form below tries to ensure.
        (cffi:with-foreign-objects ((arg-types '(:pointer :char)
                                               (the fixnum (length args)))
                                    (objc-arg-ptrs '(:pointer :void)
                                                   real-argc)
                                    (return-value-cell (if (eq return-c-type :void)
                                                           :int
                                                           return-c-type))
                                    (objc-arg-buffer +pessimistic-allocation-type+
                                                     real-argc))
          (dotimes (i real-argc)
            (setf (cffi:mem-aref objc-arg-ptrs '(:pointer :void) i)
                  (cffi:inc-pointer objc-arg-buffer
                                    (* i +pessimistic-allocation-size+))))
          (macrolet ((argref (type num)
                       `(cffi:mem-ref objc-arg-buffer ,type
                                      (* ,num +pessimistic-allocation-size+))))
            (flet ((ad-hoc-arglist->objc-arglist! (args)
                     (setf (argref '(:pointer :void) 0)
                           (pointer-to receiver)
                           (argref '(:pointer :void) 1)
                           (pointer-to selector))
                     (loop for arg in args
                           for i from 0 to raw-argc
                           do (let* ((type-name (lisp-value->type-name arg)))
                                (typecase arg
                                  ((or c-pointer-wrapper
                                       c-pointer)
                                   (setf (argref :pointer (+ i 2))
                                         (typecase arg
                                           (c-pointer-wrapper (pointer-to arg))
                                           (t arg))))
                                  (string
                                   (setf (argref :string (+ i 2))
                                         (alloc-string-and-register arg)))
                                  (t (setf (argref (type-name->c-type type-name)
                                                   (+ i 2))
                                           arg)))
                                (setf (cffi:mem-aref arg-types '(:pointer :char) i)
                                      (alloc-string-and-register
                                       (type-name->type-id type-name)))))))
              (ad-hoc-arglist->objc-arglist! args)
              (let* ((return-type-cell (alloc-string-and-register
                                        (type-name->type-id return-type)))
                     (error-cell
                      (%objcl-invoke-with-types raw-argc
                                                return-type-cell
                                                arg-types
                                                return-value-cell
                                                objc-arg-ptrs)))
                (unless (cffi:null-pointer-p error-cell)
                  ;; Note that we do not FOREIGN-FREE the error cell,
                  ;; because it is either a null pointer or a pointer to
                  ;; an Objective-C object.  In the latter case,
                  ;; INITIALIZE-INSTANCE does the memory management for
                  ;; us.
                  (error (make-condition 'exception :pointer error-cell)))
                (case return-type
                  ((id objc-class exception selector)
                   (let ((*skip-retaining*
                          (or *skip-retaining*
                              (constructor-name-p (selector-name selector)))))
                     (make-instance return-type
                        :pointer (cffi:mem-ref return-value-cell
                                               return-c-type))))
                  ((:void) (values))
                  (otherwise (cffi:mem-ref return-value-cell
                                           return-c-type)))))))))))


(defun retrieve-method-signature-info (class selector)
  (let* ((signature (primitive-invoke class
                                      :instance-method-signature-for-selector
                                      'id
                                      selector))
         (argc (primitive-invoke signature 'number-of-arguments :unsigned-int))
         (method-return-typestring (primitive-invoke signature
                                                     'method-return-type
                                                     :string))
         (method-return-type (parse-typespec method-return-typestring))
         (method-arg-typestrings (loop for x from 0 below argc
                                       collect (primitive-invoke
                                                         signature
                                                         :get-argument-type-at-index
                                                         :string
                                                         x)))
         (method-arg-types (mapcar #'parse-typespec method-arg-typestrings)))
    (values argc
            method-return-typestring
            method-return-type
            method-arg-typestrings
            method-arg-types)))


(defun typespec->c-type (typespec)
  (case (car typespec)
    ((:pointer struct union id objc-class exception array) :pointer)
    ((:string) :string)
    (otherwise (car typespec))))


(defun low-level-invoke (receiver selector return-typestring return-type
                         arg-typestrings arg-types argc args)
  (let ((return-c-type (typespec->c-type return-type))
        (arg-c-types (mapcar #'typespec->c-type arg-types)))
    (with-foreign-string-pool (register-temporary-string)
      (flet ((alloc-string-and-register (string)
               (register-temporary-string
                (cffi:foreign-string-alloc string))))
      (cffi:with-foreign-objects ((objc-arg-typestrings :string
                                                        (- argc 2))
                                  (objc-arg-ptrs :pointer argc)
                                  (objc-return-value-cell
                                   (if (eq return-c-type :void)
                                       :int
                                       return-c-type))
                                  (objc-arg-buffer +pessimistic-allocation-type+
                                                   argc))
        ;; Prepare the argument pointer vector.
        (loop for i from 0 below argc
              do (setf (cffi:mem-aref objc-arg-ptrs :pointer i)
                       (cffi:inc-pointer objc-arg-buffer
                                         (* i +pessimistic-allocation-size+))))
        (macrolet ((argref (type num)
                     `(cffi:mem-ref objc-arg-buffer ,type
                                    (* ,num +pessimistic-allocation-size+))))
          ;; Prepare the arguments.
          (setf (argref :pointer 0) (pointer-to receiver))
          (setf (argref :pointer 1) (pointer-to selector))
          (loop for i from 2
                for arg in args
                for arg-type in (cddr arg-types) ;skip the first two arguments
                for arg-c-type in (cddr arg-c-types) ;likewise
                do (case (car arg-type)
                     ((:pointer)
                      (setf (argref :pointer i) arg))
                     ((id objc-class exception)
                      (setf (argref :pointer i) (pointer-to arg)))
                     (:string
                      (setf (argref :string i)
                            (alloc-string-and-register arg)))
                     ((array struct union) (error "~A: Not implemented."
                                                  arg-type))
                     (t (setf (argref arg-c-type i) arg)))))
        ;; Prepare the argument typestring vector.
        (loop for i from 0
              for arg-typestring in arg-typestrings
              do (setf (mem-aref objc-arg-typestrings :string i)
                       (alloc-string-and-register arg-typestring)))
        (let* ((error-cell
                (%objcl-invoke-with-types (- argc 2)
                                          return-typestring
                                          objc-arg-typestrings
                                          objc-return-value-cell
                                          objc-arg-ptrs)))
          (unless (cffi:null-pointer-p error-cell)
            (error (make-condition 'exception :pointer error-cell)))
          (case (car return-type)
            ((id objc-class exception selector)
             (let ((*skip-retaining*
                    (or *skip-retaining*
                        (constructor-name-p (selector-name selector)))))
               (make-instance (car return-type)
                  :pointer (cffi:mem-ref objc-return-value-cell
                                         return-c-type))))
            ((:void) (values))
            (otherwise (cffi:mem-ref objc-return-value-cell
                                     return-c-type)))))))))


(defun invoke-with-conversion (receiver method-name &rest args)
  (let* ((selector (selector method-name))
         (class (primitive-invoke receiver 'class 'objc-class)))
    (multiple-value-bind (argc
                          method-return-typestring
                          method-return-type
                          method-arg-typestrings
                          method-arg-types)
        (retrieve-method-signature-info class selector)
      (assert (= argc (+ 2 (length args)))
              (args)
              "Wrong number of arguments (expected ~A, got ~A)."
              argc (+ 2 (length args)))
      (low-level-invoke receiver
                        selector
                        method-return-typestring
                        method-return-type
                        method-arg-typestrings
                        method-arg-types
                        argc
                        args))))


;; Optimise constant method names away by converting them to selectors
;; at load-time.
(define-compiler-macro primitive-invoke (&whole form
                                         receiver method-name return-type
                                         &rest args)
  (if (and (constantp method-name)
           (not (and (listp method-name)
                     (eq 'load-time-value (car method-name)))))
      `(primitive-invoke ,receiver
                         (load-time-value (handler-case
                                              (selector ,method-name)
                                            (serious-condition ()
                                              (warn
                                               (make-condition
                                                'style-warning
                                                :format-control
                                                "~S designates an unknown ~
                                                 method selector."
                                                :format-arguments
                                                (list ,method-name)))
                                              ,method-name)))
                         ,return-type ,@args)
      form))


;; Do the same optimisations for INVOKE-WITH-CONVERSION as for
;; PRIMITIVE-INVOKE.
(define-compiler-macro invoke-with-conversion (&whole form
                                               receiver method-name
                                               &rest args)
  (if (and (constantp method-name)
           (not (and (listp method-name)
                     (eq 'load-time-value (car method-name)))))
      `(invoke-with-conversion
        ,receiver
        (load-time-value (handler-case
                             (selector ,method-name)
                           (serious-condition ()
                             (warn
                              (make-condition 'style-warning
                                              :format-control
                                              "~S designates an unknown ~
                                               method selector."
                                              :format-arguments
                                              (list ,method-name)))
                             ,method-name)))
        ,@args)
      form))


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
