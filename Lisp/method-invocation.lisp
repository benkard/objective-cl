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


;;; (@* "Method invocation")
(defun invoke (receiver message-start &rest message-components)
  "Send a message to an Objective-C instance.

## Arguments and Values:

*receiver* --- an Objective-C wrapper object.

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
single *message name component*.  The *message name components* are in
turn concatenated in order to form the *message name* which is used as
if as the second **argument** to __invoke-by-name__.


## Examples:

    (invoke (find-objc-class 'ns-string)
            :string-with-u-t-f-8-string \"Mulk.\")
      ;=> #<GSCBufferString `Mulk.' {5B36087}>

    (invoke (find-objc-class 'ns-string)
            '|:stringWithUTF8String| \"Mulk.\")
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

    #.(setq \\*readtable\\* (copy-readtable)) 
    #.(setf (readtable-case \\*readtable\\*) :invert)
    (invoke (find-objc-class 'ns-string)
            :stringWithCString \"Mulk.\"
            :encoding 4)
      ;=> #<GSCBufferString `Mulk.' {5B36087}>


## Note:

Setting the **readtable case** of the **current readtable** to `:INVERT`
is a good way of making the Lisp system behave as traditionally as
possible while making Objective-C method names case-sensitive.

On the other hand, writing all method names in lower case while
separating parts by hyphens works nicely in all of the `:INVERT`,
`:UPCASE`, `:DOWNCASE`, and `:PRESERVE` modes as well as Allegro CL's
*modern mode*.


## Note 2:

Instead of using __invoke__, which is neither macro-friendly nor very
useful for method selection at run-time, you may **funcall** selectors
directly.  Naturally, __apply__ works as well.

The following calls are all equivalent:

    (invoke-by-name instance \"stringWithCString:encoding:\" \"Mulk.\" 4)
    (invoke instance :string-with-c-string \"Mulk.\" :encoding 4)
    (funcall (selector \"stringWithCString:encoding:\") instance \"Mulk.\" 4)
    (apply (selector \"stringWithCString:encoding:\") (list instance \"Mulk.\" 4))


## See also:

  __invoke-by-name__"

  (multiple-value-bind (message arglist)
      (split-method-call message-start message-components)
    (apply #'invoke-by-name receiver message arglist)))


(defun invoke-by-name (receiver method-name &rest args)
  "Send a message to an Objective-C object by the name of the method.

## Arguments and Values:

*receiver* --- an Objective-C wrapper object.

*method-name* --- a *selector designator*.

*args* --- a list of **object**s.

Returns: *result* --- the return value of the method invocation.


## Description:

__invoke-by-name__ is like __invoke__ except in its syntax.  It sends
the message whose selector is designated by *method-name*, which must be
either a *string*, a *symbol*, a list of message name components as in a
call to __invoke__, or an object of *type* __selector__, to *receiver*.


## Examples:

    (invoke-by-name (find-objc-class 'ns-string)
                    '(:string-with-u-t-f-8-string) \"Mulk.\")
      ;=> #<GSCBufferString `Mulk.' {5B36087}>

    (invoke-by-name (find-objc-class 'ns-object)
                    \"self\")
      ;=> #<NSObject `NSObject' {16ECF598}>

    (invoke-by-name (find-objc-class 'ns-string)
                    \"stringWithCString:encoding:\"
                    \"Mulk.\"
                    4)
      ;=> #<GSCBufferString `Mulk.' {5B36087}>


## Note:

__selector__ objects are funcallable.  Therefore, the following calls
are all equivalent:

    (invoke-by-name instance \"stringWithCString:encoding:\" \"Mulk.\" 4)
    (invoke instance :string-with-c-string \"Mulk.\" :encoding 4)
    (funcall (selector \"stringWithCString:encoding:\") instance \"Mulk.\" 4)

In fact, using __invoke-by-name__ is discouraged in favour of the latter
form.


## Rationale:

Whereas __invoke__ tries to make writing as well as reading method
invocations easy by interspersing method name components with arguments
as Objective-C does, __invoke-by-name__ is better suited for method
selection at run time as well as code generation.  It is also slightly
easier to use with __apply__.


## See also:

  __invoke__"

  ;; TODO: Support varargs.
  (let* ((selector (if (typep method-name 'selector)
                       method-name
                       (find-selector method-name)))
         (class (object-get-class receiver)))
    (multiple-value-bind (argc
                          method-return-typestring
                          method-return-type
                          method-arg-typestrings
                          method-arg-types)
        (retrieve-method-signature-info class selector
                                        (if (object-is-class-p receiver)
                                            :class
                                            :instance))
      (assert (= argc (+ 2 (length args)))
              (args)
              "Wrong number of arguments (expected ~A, got ~A)."
              argc (+ 2 (length args)))
      (low-level-invoke receiver
                        selector
                        (null-pointer)
                        method-return-typestring
                        method-return-type
                        method-arg-typestrings
                        method-arg-types
                        argc
                        args))))


(defun split-method-call (message-start message-components)
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
        (values (nreverse message-list)
                (nreverse arglist)))))


(defun primitive-invoke (receiver method-name return-type &rest args)
  (flet ((ad-hoc-value->typespec (arg)
           (etypecase arg
             ;; According to Allegro CL, strings
             ;; are POINTERP (and thus elements of
             ;; the C-POINTER type), so they have
             ;; to come first in this TYPECASE
             ;; form.  Weird.
             ;;
             ;; By the way, pointers are
             ;; represented as integers in Allegro
             ;; CL, so all integers are POINTERP,
             ;; too.
             (string '(:string ()))
             (selector '(selector ()))
             (c-pointer-wrapper '(id ()))
             (c-pointer '(:pointer ()))
             (integer '(:int ())))))
    (let ((return-typespec `(,return-type ()))
          (arg-typespecs (list* '(id ())
                                '(selector ())
                                (mapcar #'ad-hoc-value->typespec args))))
      (low-level-invoke receiver
                        (selector method-name)
                        (null-pointer)
                        (print-typespec-to-string return-typespec)
                        return-typespec
                        (mapcar #'print-typespec-to-string arg-typespecs)
                        arg-typespecs
                        (+ 2 (length args))
                        args))))


(define-cached-function retrieve-method-signature-info
    (class selector &optional (instance-or-class :instance))
    (cons (cffi:pointer-address (pointer-to class))
          (cffi:pointer-address (pointer-to selector)))
  (let* ((signature
          (objc-or (if (eq instance-or-class :instance)
                       (primitive-invoke class
                                         "instanceMethodSignatureForSelector:"
                                         'id
                                         selector)
                       (primitive-invoke class
                                         "methodSignatureForSelector:"
                                         'id
                                         selector))
                   (error (make-condition 'message-not-understood
                                          :class class
                                          :selector selector))))
         (argc (primitive-invoke signature "numberOfArguments" :unsigned-int))
         (method-return-typestring (primitive-invoke signature
                                                     "methodReturnType"
                                                     :string))
         (method-return-type (parse-typespec method-return-typestring t))
         (method-arg-typestrings (loop for x from 0 below argc
                                       collect (primitive-invoke
                                                         signature
                                                         "getArgumentTypeAtIndex:"
                                                         :string
                                                         x)))
         (method-arg-types (mapcar #'parse-typespec method-arg-typestrings)))
    (values argc
            method-return-typestring
            method-return-type
            method-arg-typestrings
            method-arg-types)))


(defun typespec->c-type (typespec)
  (case (typespec-primary-type typespec)
    ((:pointer pointer struct union id objective-c-class exception array
      selector :id :class :exception :selector)
     :pointer)
    ((:string) :string)
    (otherwise (typespec-primary-type typespec))))


(defun low-level-invoke (receiver selector superclass-pointer-for-send-super
                         return-typestring return-type
                         arg-typestrings arg-types argc args)
  (when (object-is-class-p receiver)
    (foreign-class-ensure-registered receiver))
  (let ((return-c-type (typespec->c-type return-type))
        (arg-c-types (mapcar #'typespec->c-type arg-types)))
    (with-foreign-string-pool (register-temporary-string
                               allocate-string-and-register)
      (cffi:with-foreign-objects ((objc-arg-typestrings :string
                                                        (- argc 2))
                                  (objc-arg-ptrs :pointer argc)
                                  (objc-return-value-cell
                                   ;; Note that this cell is not used if
                                   ;; the method returns a struct, array
                                   ;; or union.  For these, see
                                   ;; OBJC-STRUCT-RETURN-VALUE-CELL
                                   ;; below.
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
        ;; Prepare the argument typestring vector.  Note that we don't
        ;; pass the first two strings, as they are always the same.
        (loop for i from 0
              for arg-typestring in (cddr arg-typestrings)
              do (setf (mem-aref objc-arg-typestrings :string i)
                       (allocate-string-and-register arg-typestring)))
        (macrolet ((argref (type num)
                     `(cffi:mem-ref objc-arg-buffer ,type
                                    (* ,num +pessimistic-allocation-size+))))
          ;; Prepare the arguments.
          (setf (argref :pointer 0) (if (pointerp receiver)
                                        receiver
                                        (pointer-to receiver)))
          (setf (argref :pointer 1) (if (pointerp selector)
                                        selector
                                        (pointer-to selector)))
          (loop for i from 2
                for arg in args
                for arg-type in (cddr arg-types) ;skip the first two arguments
                for arg-c-type in (cddr arg-c-types) ;likewise
                do (case (car arg-type)
                     ((:pointer)
                      (setf (argref :pointer i) arg))
                     ((objective-c-class exception)
                      (setf (argref :pointer i) (pointer-to arg)))
                     ((selector)
                      (setf (argref :pointer i) (pointer-to (selector arg))))
                     ((:string)
                      (setf (argref :string i)
                            (allocate-string-and-register arg)))
                     ((struct union)
                      ;; This is not very sophisticated, but, at
                      ;; present, we don't care about the internals of
                      ;; structs and unions much.  Functions returning
                      ;; structs actually just give us pointers to them,
                      ;; so we just put those pointers back into the
                      ;; functions as arguments.
                      ;;
                      ;; Note that the target type is a struct/union,
                      ;; not a pointer.  This means that we actually
                      ;; have to pass a struct/union as an argument.  We
                      ;; therefore ignore the memory space reserved for
                      ;; argument cells in the argument buffer and
                      ;; simply set the argument pointer directly.
                      (setf (cffi:mem-aref objc-arg-ptrs :pointer i)
                            arg))
                     ((array)
                      (error "Method ~A of object ~A tried to accept an array ~
                              as argument #~D.  It must be mistaken."
                             selector receiver i))
                     ((id)
                      ;; This case is actually interesting.  We can do a
                      ;; lot of automatic conversion between different
                      ;; kinds of stuff.  The conversion rules are
                      ;; somewhat arbitrary, but in the absence of more
                      ;; detailed method signature type information,
                      ;; it's the best we can do.
                      (setf (argref arg-c-type i)
                            (pointer-to (coerce-object arg 'id))))
                     (t (setf (argref arg-c-type i)
                              (case arg
                                ;; Do the right thing for booleans.
                                ;;
                                ;; Note that Objective-C method
                                ;; invocations do not understand
                                ;; generalised booleans.  Among other
                                ;; things, this means that passing 0 for
                                ;; a boolean is the same as passing NIL,
                                ;; not the same as passing T.
                                ((nil) 0)
                                ((t) 1)
                                (otherwise arg)))))))
        (let* ((objc-struct-return-value-cell
                (if (member (typespec-primary-type return-type)
                            '(struct union array))
                    ;; Note that sizeof(char) is defined to be 1.  That
                    ;; is, sizeof returns a size in units of chars, not
                    ;; in units of bytes.
                    (foreign-alloc :char :count (%objcl-sizeof-type
                                                 return-typestring))
                    nil))
               (error-cell
                (%objcl-invoke-with-types (- argc 2)
                                          superclass-pointer-for-send-super
                                          return-typestring
                                          objc-arg-typestrings
                                          (or objc-struct-return-value-cell
                                              objc-return-value-cell)
                                          objc-arg-ptrs)))
          (unless (cffi:null-pointer-p error-cell)
            (error (make-condition 'exception :pointer error-cell)
                   #+(or) (intern-pointer-wrapper 'exception :pointer error-cell)))
          (when (eq (typespec-primary-type return-type) 'array)
            (error "Method ~A of object ~A tried to return an array.  ~
                    It must be mistaken."
                   selector receiver))
          (convert-from-foreign-value (or objc-struct-return-value-cell
                                          objc-return-value-cell)
                                      return-type
                                      (or *skip-retaining*
                                          (constructor-name-p
                                           (selector-name selector)))
                                      (returned-char-is-bool-p receiver
                                                               selector)))))))


;;; (@* "Helper functions")
(defun constructor-name-p (method-name)
  (flet ((method-name-starts-with (prefix)
           (let ((mismatch (mismatch method-name prefix)))
             (or (not mismatch)
                 (>= mismatch (length prefix))))))
    (or (method-name-starts-with "alloc")
        (method-name-starts-with "new"))))


;;; (@* "High-level Data Conversion")
(defgeneric coerce-object (object type))


(defcoercion id ((x id))
  x)

(defcoercion id ((x objective-c-class))
  x)

(defcoercion id ((x exception))
  x)

(defcoercion id ((x integer))
  (primitive-invoke (find-objc-class 'ns-number)
                    "numberWithInt:"
                    'id
                    x))

(defcoercion id ((x float))
  (primitive-invoke (find-objc-class 'ns-number)
                    (etypecase x
                      (long-float "numberWithDouble:")
                      (double-float "numberWithDouble:")
                      (short-float "numberWithFloat:")
                      (single-float "numberWithFloat:"))
                    'id
                    x))

(defcoercion id ((x string))
  (primitive-invoke (find-objc-class 'ns-string)
                    "stringWithUTF8String:"
                    'id
                    x))

(defcoercion id ((x list))
  ;; Circular lists may cause this to hang.  So may lists that contain
  ;; themselves, as well as lists that contain other data structures
  ;; that contain themselves or this list, and so on.
  (apply #'primitive-invoke
         (find-objc-class 'ns-array)
         "arrayWithObjects:"
         'id
         (append (mapcar #'(lambda (element)
                             (coerce-object element 'id))
                         x)
                 (list +nil+))))


(defcoercion class ((x id))
  (object-get-class x))

(defcoercion class ((x exception))
  (object-get-class x))

(defcoercion class ((x objective-c-class))
  x)

(defcoercion class ((x string))
  (find-objc-class x t))

(defcoercion class ((x symbol))
  (find-objc-class x t))


(defcoercion integer ((x integer))
  x)

(defcoercion integer ((x id))
  (assert (objc-typep x 'ns-number))
  (invoke x 'int-value))

(defcoercion integer ((x number))
  (truncate x))

(defcoercion integer ((x null))
  (declare (ignore x))
  +no+)

(defcoercion integer (x)
  (declare (ignore x))
  +yes+)


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


;; Note that this refers to the Objective-C BOOL type, not the Lisp
;; BOOLEAN type.
(defcoercion bool ((x null))
  (declare (ignore x))
  +no+)

(defcoercion bool (x)
  (declare (ignore x))
  +yes+)


;; Note that this refers to the Lisp BOOLEAN type, not the Objective-C
;; BOOL type.
(defcoercion boolean ((x number))
  (not (zerop x)))


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
