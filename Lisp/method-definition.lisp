;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007, 2008  Matthias Andreas Benkard.
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

(in-package #:objective-cl)


(defclass objective-c-generic-function (standard-generic-function)
     ()
  (:metaclass funcallable-standard-class)
  (:documentation "An Objective-C dispatch function for a given selector.

## Description:

Every Lisp-defined __objective-c-method__ belongs to a corresponding
__objective-c-generic-function__ that handles **method combination** and
registration of newly added **method**s with the Objective-C runtime.

It is recommended to create all __objective-c-generic-functions__ by
means of the macro __define-objective-c-generic-function__.

If you subclass this **class**, be aware that there is no protocol that
describes its behaviour, so the consequences of overriding any
**generic function**s **applicable** to the **class** are undefined."))


(defclass objective-c-method (standard-method)
     ((return-type :initarg :return-type
                   :accessor method-return-type)
      (argument-types :initarg :argument-types
                      :accessor method-argument-types))
  (:metaclass standard-class)
  (:documentation "An Objective-C method implemented in Lisp.

## Description:

Instances of __objective-c-method__ are similar to instances of
__standard-method__ except that they know about their foreign argument
and return types and recognise different **qualifier**s that they
communicate to the __objective-c-generic-function__ that they belong to.

It is recommended to create all __objective-c-methods__ by means of the
macro __define-objective-c-method__.

There is no protocol that defines the behaviour of this **class**, so
the consequences of subclassing it and overriding its **method**s is
undefined."))


(defun qualifiers-return-type (qualifiers)
  (find-if #'(lambda (x)
               (or (not (keywordp x))
                   (typep x 'objective-c-type-keyword)))
           qualifiers))


(defmacro defobjcmethod (name &rest args)
  `(define-objective-c-method ,name ,@args))


(defmacro define-objective-c-method (name &rest args)
  "Define a new Objective-C method.

## Arguments and Values:

*name* --- a *symbol*.

*qualifier* --- a **method qualifier**.

*return-type* --- a *typespec*.

*lambda-list* --- a **modified lambda list**.

*body* --- an **implicit progn**.


## Description:

_define-objective-c-generic-function_ is like __defgeneric__ except in the
following aspects:

1. *name* is immediately replaced by a *symbol* **intern**ed in package
   _objective-c-methods_.

2. The default value for the _:generic-function-class_ option is
   _objective-cl:objective-c-generic-function_.

3. The default value for the _:method-class_ option is
   _objective-cl:objective-c-method_.

_define-objective-c-generic-function_ recognises the same *options* as
__defgeneric__, including _:generic-function-class_ and _:method-class_.

The **lexical environment** of *body* is augmented to include the
function __super__.


## Example:

    #.(enable-method-syntax)

    (define-objective-c-class ns::mlk-my-class (ns::ns-object)
         ((foos :initargs :foos)
          (foo-count :foreign-type :int)))
      => NS::MLK-MY-CLASS

    (define-objective-c-generic-function #/foo:bar:stuff:do: (self y z a b))
      => #<OBJECTIVE-C-GENERIC-FUNCTION OBJECTIVE-C-METHODS::|foo:bar:stuff:do:| (0)>

    (define-objective-c-method #/foo:bar:stuff:do: :int
        ((self ns::mlk-my-class) (y :int) z a (b ns::ns-number))
      (format t \"Hello!  Z and A are ~A and~
                  ~&~A, respectively.~
                  ~&Have a nice day.\" z a)
      (+ y 20))
      => #<OBJECTIVE-C-METHOD OBJECTIVE-C-METHODS::|foo:bar:stuff:do:| ((EQL NS:MLK-MY-CLASS) NS:MLK-MY-CLASS T T T NS:NS-NUMBER) {CE8E531}>

    (#/foo:bar:stuff:do: (#/new (find-objc-class 'ns::mlk-my-class))
                         100
                         30
                         \"FOO!\"
                         5)
      => Output:
           Hello!  Z and A are #<NS:NS-INT-NUMBER `30' {82F1DC0}> and
           #<NS:MLK-LISP-STRING `FOO!' {82F2190}>, respectively.
           Have a nice day.
      => 120

    #.(disable-method-syntax)


## Note 1:

It is not generally possible to define methods after a class has already
been registered with the Objective-C runtime.  The latter inevitably
happens when Objective-CL first sees an instance of the class.


## Note 2:

If you do not call __define-objective-c-generic-function__ before using
__define-objective-c-method__, it will be called implicitly by the
latter.  There is nothing wrong with relying on this; in fact, if you do
not want to set any options for the generic function, your code will
probably seem less cramped if you leave the redundant
__define-objective-c-generic-function__ calls out.


## Note 3:

It is customary to use the #/ notation enabled by
__enable-method-syntax__ to write method names for
__define-objective-c-generic-function__.


## Note 4:

Do not call __call-next-method__ in the body of an
__objective-c-method__.  Its behaviour is quite different from
__super__, ill-defined, and probably not desirable anyway.

Use __super__ instead.


## See also:

  __define-objective-c-generic-function__, __define-objective-c-class__,
__super__"
  (let ((qualifiers (list)))
    (loop until (listp (first args))
          do (push (pop args) qualifiers))
    (setq qualifiers (nreverse qualifiers))
    (destructuring-bind (lambda-list . body) args
      (let ((lambda-list (copy-list lambda-list)))
        (loop for arg-cons on lambda-list
              for arg = (car arg-cons)
              until (member arg lambda-list-keywords)
              if (listp arg)
                if (typep (second arg) 'objective-c-type-keyword)
                  collect (second arg) into type-specifiers
                  and collect (first arg) into arg-names
                  and do (setf (car arg-cons) (first arg))
                else
                  ;; We simply map all non-OBJECTIVE-C-TYPE-KEYWORD
                  ;; specialisers to :ID.  This makes sense: If the
                  ;; specialiser is an Objective-C type, the type
                  ;; specifier should obviously be :ID.  If it's a
                  ;; non-Objective-C CLOS class, we're going to pass
                  ;; Objective-C objects of the LISP-VALUE-WRAPPER-MIXIN
                  ;; kind to the method, whose type specifier is :ID as
                  ;; well.
                  collect :id into type-specifiers
                  and collect (first arg) into arg-names
              else
                collect :id into type-specifiers
                and collect arg into arg-names
              finally (let ((super-args-sym (gensym))
                            (captured-args-sym (gensym))
                            (class-arg-sym (gensym))
                            (class-name (intern (symbol-name (cadar lambda-list))
                                                '#:objective-c-classes))
                            (real-name (intern (symbol-name name)
                                               '#:objective-c-methods)))
                        (return
                          `(progn
                             (eval-when (:load-toplevel :execute)
                               (unless (fboundp ',real-name)
                                 ;; Note that we need not specify a
                                 ;; :LAMBDA-LIST here, as not supplying
                                 ;; it means it's simply going to be
                                 ;; initialised when a method is first
                                 ;; added to the generic function.
                                 ;;
                                 ;; For some reason,
                                 ;; ENSURE-GENERIC-FUNCTION raises an
                                 ;; error on Allegro CL claiming that
                                 ;; MAKE-INSTANCE of
                                 ;; OBJECTIVE-C-GENERIC-FUNCTION does
                                 ;; not understand the
                                 ;; :GENERIC-FUNCTION-CLASS initarg.
                                 ;; Calling
                                 ;; ENSURE-GENERIC-FUNCTION-USING-CLASS
                                 ;; instead does not display this
                                 ;; behaviour.  Weird.
                                 (ensure-generic-function-using-class
                                  nil
                                  ',real-name
                                  :generic-function-class
                                    (find-class 'objective-c-generic-function)
                                  :method-class
                                    (find-class 'objective-c-method))))
                             (defmethod ,real-name
                                 argtypes-start ,@type-specifiers argtypes-end
                                 ,@qualifiers
                                 ((,class-arg-sym (eql ',class-name))
                                  ,@lambda-list)
                               (let ((,captured-args-sym (list ,@arg-names)))
                                 (flet ((super (&rest ,super-args-sym)
                                          (invoke-by-name-super-v
                                           (first ,captured-args-sym)
                                           ,(generic-function-name->method-name
                                             name)
                                           (objcl-class-superclass
                                            (find-objc-class ',class-name))
                                           (or ,super-args-sym
                                               (rest ,captured-args-sym)))))
                                   (declare (ignorable (function super)))
                                   ,@body)))))))))))


(defun super (&rest args)
  "Send a super message to the receiver of the current message.

## Syntax:

*args* --- a **list**.


## Description:

A _super_ call is only valid inside the body of a **method** defined by
means of __define-objective-c-method__.  In this case, the Objective-C
**method** of the same name defined in the superclass of the class
specialised over by the method is called.  In other words, a super call
is made.

The behaviour is similar in spirit to calling __call-next-method__ in
the body of a __standard-method__ except that it will be done by the
Objective-C runtime, thereby ignoring any pure-Lisp superclasses.
__super__ is therefore single-inheritance only.

If __super__ is called with no arguments, the original method arguments
are passed to the super method.  Otherwise, the supplied arguments are
passed.


## Note:

Do not call __call-next-method__ in the body of an
__objective-c-method__.  Its behaviour is quite different from
__super__, ill-defined, and probably not desirable anyway.


## Examples:

    (define-objective-c-method #/characterAtIndex: :short ((self ns::my-string) (index :unsigned-long))
      (if (weird-string-p self)
          (super (1+ index))
          (super)))"
  (declare (ignore args))
  (error "Tried to call ~S outside an Objective-C method." 'super))


(defmacro defobjcgeneric (name lambda-list &body options)
  "Define a new Objective-C generic function.

## Arguments and Values:

*name* --- a *symbol*.

*lambda-list* --- a **generic function lambda list**.

*options* --- a *list* (not evaluated).


## Description:

This macro is equivalent to __define-objective-c-generic-function__.


## See also:

  __define-objective-c-generic-function__, __defobjcmethod__"
  `(define-objective-c-generic-function ,name ,lambda-list ,@options))


(defmacro define-objective-c-generic-function (name lambda-list &body options)
  "Define a new Objective-C generic function.

## Arguments and Values:

*name* --- a *symbol*.

*lambda-list* --- a **generic function lambda list**.

*options* --- a *list* (not evaluated).


## Description:

_define-objective-c-generic-function_ is like __defgeneric__ except in the
following aspects:

1. *name* is immediately replaced by a *symbol* **intern**ed in package
   _objective-c-methods_.

2. The default value for the _:generic-function-class_ option is
   _objective-cl:objective-c-generic-function_.

3. The default value for the _:method-class_ option is
   _objective-cl:objective-c-method_.

_define-objective-c-generic-function_ recognises the same *options* as
__defgeneric__, including _:generic-function-class_ and _:method-class_.


## Example:

See __define-objective-c-method__.


## Note:

It is customary to use the #/ notation enabled by
__enable-method-syntax__ to write method names for
__define-objective-c-generic-function__.


## See also:

  __define-objective-c-method__, __define-objective-c-class__"
  `(defgeneric ,(intern (symbol-name name) '#:objective-c-methods)
       (,(gensym "CLASS") ,@lambda-list)
     ,@(unless (position :generic-function-class
                         options
                         :key #'car)
         `((:generic-function-class objcl:objective-c-generic-function)))
     ,@(unless (position :method-class
                         options
                         :key #'car)
         `((:method-class objcl:objective-c-method)))
     ,@options))


(defvar *callback-names* (make-hash-table :test #'eql))

(defun intern-callback-name (method)
  (or (gethash method *callback-names* nil)
      (setf (gethash method *callback-names* nil)
            (intern (format nil "~A ~S ~A"
                            (generic-function-name
                             (method-generic-function method))
                            (class-name (second (method-specializers method)))
                            (sort (copy-list (method-qualifiers method))
                                  #'string<
                                  :key #'string))
                    '#:objective-cl))))


(defmethod add-method :after ((gf objective-c-generic-function)
                              (method objective-c-method))
  ;; FIXME: Support &REST arguments.
  (let* ((class (second (method-specializers method)))
         (method-name (generic-function-name->selector
                       (generic-function-name gf)))
         (registered-p (foreign-class-registered-p class))
         (return-type (method-return-type method))
         (method-argument-types (method-argument-types method))
         (argument-types (list* (first method-argument-types)
                                :selector
                                (rest method-argument-types)))
         (return-typestring (print-typespec-to-string return-type))
         (arg-typestrings (mapcar #'print-typespec-to-string
                                  argument-types))
         (callback-name (intern-callback-name method))
         (arg-symbols (mapcar #'(lambda (x)
                                  (declare (ignore x))
                                  (gensym "ARG"))
                              argument-types)))
    (eval (loop for type in argument-types
                for symbol in arg-symbols
                collect (list symbol (typespec->c-type type)) into cffi-lambda-list
                if (member (typespec-primary-type type) '(:id :class :selector))
                  collect `(intern-pointer-wrapper ',type :pointer ,symbol)
                    into arguments
                else
                  collect symbol into arguments
                finally (return
                          `(defcallback ,callback-name
                               ,(typespec->c-type return-type)
                               ,cffi-lambda-list
                             (declare (ignorable ,(cadr arg-symbols)))
                             (unwind-protect
                                 (,(if (member (typespec-primary-type return-type)
                                               '(:id :class :selector))
                                       'pointer
                                       'progn)
                                  (coerce-object
                                   (,(generic-function-name gf)
                                     ;; Pass the class this method is
                                     ;; being defined for as the first
                                     ;; argument.  This is needed so that
                                     ;; super calls can work.
                                     ',(class-name class)
                                     ;; Leave the second argument (the
                                     ;; selector) out.
                                     ,@(list* (car arguments) (cddr arguments)))
                                   ',return-type))
                               ;; FIXME: We may want to wrap signalled
                               ;; SERIOUS-CONDITIONS in some kind of
                               ;; Objective-C exception object and put
                               ;; it into *OBJCL-CURRENT-EXCEPTION*.  Or
                               ;; maybe we don't, assuming the Lisp
                               ;; system can handle a few layers of C
                               ;; functions between a condition's
                               ;; signalling and handling, in which case
                               ;; we'd only destroy the restart
                               ;; mechanism by pseudo-handling
                               ;; conditions in this way.
                               (%objcl-acquire-lock *objcl-current-exception-lock*))))))
    (let ((callback (get-callback callback-name)))
      (with-foreign-object (arg-typestring-buffer :string
                                                  (- (length arg-typestrings) 2))
        (with-foreign-string-pool (register-temp allocate-temp)
          (loop for i from 0
                for typestring in (cddr arg-typestrings)
                do (setf (mem-aref arg-typestring-buffer :string i)
                         (allocate-temp typestring)))
          (%objcl-add-method (pointer-to class)
                             (symbol->objc-class-name (class-name class))
                             (pointer-to method-name)
                             callback
                             (- (length arg-typestrings) 2)
                             return-typestring
                             arg-typestring-buffer
                             (apply #'concatenate
                                    'string
                                    return-typestring
                                    arg-typestrings)
                             (if registered-p 1 0))))))
  #+(or) (format t "~&ADD-METHOD:~&  ~A, ~A" gf method))


(defmethod initialize-instance :around ((method objective-c-method)
                                        &rest initargs
                                        &key documentation
                                             function
                                             lambda-list
                                             qualifiers
                                             specializers
                                        &allow-other-keys)
  (declare (ignore documentation function lambda-list specializers))
  #+(or) (format t "~&INITIALIZE-INSTANCE:~&  ~S" initargs)
  (let* ((argtypes-start (position 'argtypes-start qualifiers))
         (argtypes-end (position 'argtypes-end qualifiers))
         (argument-types (subseq qualifiers (1+ argtypes-start) argtypes-end))
         (qualifiers (append (subseq qualifiers 0 argtypes-start)
                             (subseq qualifiers (1+ argtypes-end))))
         (new-initargs (copy-list initargs))
         (return-type (qualifiers-return-type qualifiers)))
    (setf (getf new-initargs :qualifiers) (remove return-type qualifiers))
    (apply #'call-next-method
           method
           :return-type (or return-type :id)
           :argument-types argument-types
           new-initargs)))


#+(or)
(defgeneric bla (x y z &rest r)
  (:generic-function-class objective-c-generic-function)
  (:method-class objective-c-method))

#+(or)
(defmethod bla :abc ((x number) (y symbol) c &rest r)
  (declare (ignore c r))
  (+ x 3))
