(in-package #:mulk.objective-cl)


;;;; (@* "Foreign data types")
(defctype char-pointer :pointer)

;; Let us just hope that two longs make a long long, space-wise.
(defcstruct double-long
  (left :long)
  (right :long))

(defcunion obj-data-union
  (id-val :pointer)
  (class-val :pointer)
  (exc-val :pointer)
  (sel-val :pointer)
  (char-val :char)
  (short-val :short)
  (int-val :int)
  (long-val :long)
  #-cffi-features:no-long-long (long-long-val :long-long)
  #+cffi-features:no-long-long (double-long-val double-long)
  (float-val :float)
  (double-val :double)
  (bool-val :boolean)
  (charptr-val :pointer)
  (ptr-val :pointer))

(defcstruct obj-data
  (type char-pointer)
  (data obj-data-union))

(defmethod translate-to-foreign ((value string) (type (eql 'char-pointer)))
  (foreign-string-alloc value))

(defmethod translate-from-foreign (c-value (type (eql 'char-pointer)))
  (foreign-string-to-lisp c-value))


;;;; (@* "Objective C object wrapper classes")
(defclass c-pointer-wrapper ()
  ((pointer :type     c-pointer
            :reader   pointer-to
            :initarg  :pointer
            :initform nil)))


(defclass selector   (c-pointer-wrapper) ()
  (:documentation "An Objective C method selector.

## Description:

Method selectors are Objective C's equivalent to what Common Lisp calls
**symbols**.  Their use is restricted to retrieving methods by name.

__selector__ objects cannot be created by means of __make-instance__.
Use __find-selector__ instead.


## See also:

  __find-selector__"))


(defclass id         (c-pointer-wrapper) ()
  (:documentation "An instance of an Objective C class.

## Description:

The class __id__ serves as a general-purpose container for all kinds of
Objective C objects that are instances of some Objective C class, that
is, neither primitive C values nor __selector__, __class__ or
__exception__ objects.

__id__ objects cannot be created by means of __make-instance__.  Use
a suitable class method instead as you would in Objective C.


## Examples:

    (invoke (find-objc-class 'ns-object)
            'self)
      ;=> #<NSObject `NSObject' {16ECF598}>

    (invoke (find-objc-class 'ns-string)
            :string-with-c-string \"Mulk.\")
      ;=> #<GSCBufferString `Mulk.' {5B36087}>

    (invoke (find-objc-class 'ns-string)
            'new)
      ;=> #<GSCBufferString `' {FFFFFFE}>


## See also:

  __invoke__, __invoke-by-name__, __exception__"))


(defclass objc-class (c-pointer-wrapper) ()
  (:documentation ""))


(define-condition exception (error)
  ((pointer :type     c-pointer
            :accessor pointer-to
            :initarg  :pointer))
  (:report (lambda (condition stream)
             (format stream
                     "The Objective C runtime has issued an exception of ~
                      type `~A'.~&~
                      Reason: ~A."
                     (invoke-by-name
                      (invoke-by-name condition "name")
                      "UTF8String")
                     (invoke-by-name
                      (invoke-by-name condition "reason")
                      "UTF8String"))))
  (:documentation "The condition type for Objective C exceptions.

## Description:

Whenever an Objective C call made by means of __invoke__ or
__invoke-by-name__ raises an exception, the exception is propagated to
the Lisp side by being encapsulated in an __exception__ object and
signaled.

Note that it is currently impossible to directly extract the original
Objective C exception from an __exception__ object, although it might
arguably be desirable to do so.  As __exception__ objects behave just
like __id__ objects in almost all circumstances, this is not much of a
problem, though.  If you really do need an __id__ instance rather than
an __exception__, you can simply send it the `self' message.


## Examples:

\(With __install-reader-syntax__ enabled.)

    (handler-case
        [NSArray selph]   ; oops, typo
      (exception (e)
        e))
      ;=> #<MULK.OBJECTIVE-CL:EXCEPTION NSInvalidArgumentException {1048D63}>

    (class-of *)
      ;=> #<CONDITION-CLASS EXCEPTION>

    (class-of [** self])
      ;=> #<STANDARD-CLASS ID>


## See also:

  __id__"))


(defgeneric objcl-eql (obj1 obj2))
(defmethod objcl-eql ((obj1 c-pointer-wrapper) (obj2 c-pointer-wrapper))
  (pointer-eq (pointer-to obj1) (pointer-to obj2)))
(defmethod objcl-eql (obj1 obj2)
  (eql obj1 obj2))


(defun dealloc-obj-data (obj-data)
  (with-foreign-slots ((type data) obj-data obj-data)
    (when (and (pointerp type)
               (not (null-pointer-p type)))
      (foreign-string-free type)))
  (foreign-free obj-data))


;;;; (@* "Convenience types")
(deftype c-pointer ()
  '(satisfies pointerp))
