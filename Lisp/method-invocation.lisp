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
            :string-with-u-t-f-8-string \"Mulk.\")
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

  (multiple-value-bind (message arglist)
      (split-method-call message-start message-components)
    (apply #'invoke-by-name receiver message arglist)))


(defun invoke-by-name (receiver method-name &rest args)
  "Send a message to an Objective C object by the name of the method.

## Arguments and Values:

*receiver* --- an Objective C wrapper object.

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


## Rationale:

Whereas __invoke__ tries to make writing as well as reading method
invocations easy by interspersing method name components with arguments
as Objective-C does, __invoke-by-name__ is better suited for method
selection at run time as well as code generation.  It is also slightly
easier to use with __apply__.


## See also:

  __invoke__"

  ;; TODO: Support varargs.
  (let* ((selector (selector method-name))
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
                                  (string
                                   (setf (argref :string (+ i 2))
                                         (alloc-string-and-register arg)))
                                  ((or c-pointer-wrapper
                                       c-pointer)
                                   (setf (argref :pointer (+ i 2))
                                         (typecase arg
                                           (c-pointer-wrapper (pointer-to arg))
                                           (t arg))))
                                  (t (setf (argref (type-name->c-type type-name)
                                                   (+ i 2))
                                           arg)))
                                (setf (cffi:mem-aref arg-types '(:pointer :char) i)
                                      (alloc-string-and-register
                                       (type-name->type-id type-name)))))))
              (ad-hoc-arglist->objc-arglist! args)
              (let ((error-cell
                     (%objcl-invoke-with-types raw-argc
                                               (type-name->type-id return-type)
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
                              (constructor-name-p (selector-name selector))))
                         (pointer (cffi:mem-ref return-value-cell
                                                return-c-type)))
                     (if (cffi:null-pointer-p pointer)
                         nil
                         (make-pointer-wrapper return-type :pointer pointer))))
                  ((:void) (values))
                  (otherwise (cffi:mem-ref return-value-cell
                                           return-c-type)))))))))))


(define-cached-function retrieve-method-signature-info
    (class selector &optional (instance-or-class :instance))
    (cons (cffi:pointer-address (pointer-to class))
          (cffi:pointer-address (pointer-to selector)))
  (let* ((signature
          (or (if (eq instance-or-class :instance)
                  (primitive-invoke class
                                    :instance-method-signature-for-selector
                                    'id
                                    selector)
                  (primitive-invoke class
                                    :method-signature-for-selector
                                    'id
                                    selector))
              (error (make-condition 'message-not-understood
                                     :class class
                                     :selector selector))))
         (argc (primitive-invoke signature 'number-of-arguments :unsigned-int))
         (method-return-typestring (primitive-invoke signature
                                                     'method-return-type
                                                     :string))
         (method-return-type (parse-typespec method-return-typestring t))
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
    ((:pointer struct union id objc-class exception array selector) :pointer)
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
                                     ;; FIXME: This won't work for
                                     ;; structs, arrays and unions!
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
                         (alloc-string-and-register arg-typestring)))
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
                       ((objc-class exception)
                        (setf (argref :pointer i) (pointer-to arg)))
                       ((selector)
                        (setf (argref :pointer i) (pointer-to (selector arg))))
                       ((:string)
                        (setf (argref :string i)
                              (alloc-string-and-register arg)))
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
                        ;; This, too, might someday be ripped out and
                        ;; replaced with something more flexible.  For
                        ;; now, it's the same as for structs and unions.
                        ;; That's the nice thing about opaque C data
                        ;; structures: As a binding writer, we just pass
                        ;; them around without caring about their
                        ;; structure.
                        (setf (cffi:mem-aref objc-arg-ptrs :pointer i)
                              arg))
                       ((id)
                        ;; This case is actually interesting.  We can do a
                        ;; lot of automatic conversion between different
                        ;; kinds of stuff.  The conversion rules are
                        ;; somewhat arbitrary, but in the absence of more
                        ;; detailed method signature type information,
                        ;; it's the best we can do.
                        (setf (argref arg-c-type i)
                              (pointer-to (coerce-object arg 'id))))
                       (t (setf (argref arg-c-type i) arg)))))
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
                 (make-pointer-wrapper (car return-type)
                    :pointer (cffi:mem-ref objc-return-value-cell
                                           return-c-type))))
              ((:void) (values))
              (otherwise (cffi:mem-ref objc-return-value-cell
                                       return-c-type)))))))))


;;; (@* "Helper functions")
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

(defcoercion id ((x objc-class))
  x)

(defcoercion id ((x exception))
  x)

(defcoercion id ((x integer))
  (primitive-invoke (find-objc-class 'ns-number)
                    :number-with-int
                    'id
                    x))

(defcoercion id ((x float))
  (primitive-invoke (find-objc-class 'ns-number)
                    (etypecase x
                      (long-float :number-with-double)
                      (double-float :number-with-double)
                      (short-float :number-with-float)
                      (single-float :number-with-float))
                    'id
                    x))

(defcoercion id ((x string))
  (primitive-invoke (find-objc-class 'ns-string)
                    :string-with-u-t-f-8-string
                    'id
                    x))

(defcoercion id ((x list))
  ;; Circular lists may cause this to hang.  So may lists that contain
  ;; themselves, as well as lists that contain other data structures
  ;; that contain themselves or this list, and so on.
  (apply #'primitive-invoke
         (find-objc-class 'ns-array)
         :array-with-objects
         'id
         (append (mapcar #'(lambda (element)
                             (coerce-object element 'id))
                         x)
                 (list +nil+))))


(defcoercion class ((x id))
  (object-get-class x))

(defcoercion class ((x exception))
  (object-get-class x))

(defcoercion class ((x objc-class))
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
