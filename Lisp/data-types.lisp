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


(defclass selector   (c-pointer-wrapper) ())
(defclass id         (c-pointer-wrapper) ())
(defclass objc-class (c-pointer-wrapper) ())


(define-condition exception (error)
  ((pointer :type     c-pointer
            :accessor pointer-to
            :initarg  :pointer))
  (:documentation "The condition type for Objective C exceptions.")
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
                      "UTF8String")))))


(defgeneric objcl-eql (obj1 obj2))
(defmethod objcl-eql ((obj1 c-pointer-wrapper) (obj2 c-pointer-wrapper))
  (pointer-eq (pointer-to obj1) (pointer-to obj2)))
(defmethod objcl-eql (obj1 obj2)
  (eql obj1 obj2))


(defun dealloc-obj-data (obj-data)
  (with-foreign-slots ((type data) obj-data obj-data)
    (foreign-string-free type))
  (foreign-free obj-data))


(defmethod print-object ((object id) stream)
  (print-unreadable-object (object stream)
    (format stream "~A `~A' {~X}"
            (objcl-class-name (invoke-by-name object "class"))
            (invoke-by-name (invoke-by-name object "description")
                            "UTF8String")
            (invoke-by-name object "hash"))))


(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (format stream "~S ~A {~X}"
            'objc-class
            (objcl-class-name class)
            (invoke-by-name class "hash"))))


(defmethod print-object ((selector selector) stream)
  (print-unreadable-object (selector stream)
    (format stream "~S `~A'"
            'selector
            (selector-name selector))))


(defmethod print-object ((exception exception) stream)
  (print-unreadable-object (exception stream)
    (format stream "~S ~A {~X}"
            'exception
            (invoke-by-name (invoke-by-name exception "name")
                            "UTF8String")
            (invoke-by-name exception "hash"))))


;;;; (@* "Convenience types")
(deftype c-pointer ()
  '(satisfies pointerp))
