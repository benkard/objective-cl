(in-package #:mulk.objective-cl)


;;;; (@* "Foreign data types")
(defctype char-pointer :pointer)

(defcunion obj-data-union
  (id-val :pointer)
  (class-val :pointer)
  (exc-val :pointer)
  (sel-val :pointer)
  (char-val :char)
  (short-val :short)
  (int-val :int)
  (long-val :long)
  (long-long-val :long-long)
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


(defclass objc-selector (c-pointer-wrapper) ())
(defclass objc-id       (c-pointer-wrapper) ())
(defclass objc-class    (c-pointer-wrapper) ())


(define-condition objc-exception (error)
  ((pointer :type     c-pointer
            :accessor pointer-to
            :initarg  :pointer))
  (:documentation "The condition type for Objective C exceptions.")
  (:report (lambda (condition stream)
             (format stream
                     "The Objective C runtime has issued an exception of ~
                      type `~A'.~&~
                      Reason: ~A."
                     (objcl-invoke-class-method
                      (objcl-invoke-class-method condition "name")
                      "UTF8String")
                     (objcl-invoke-class-method
                      (objcl-invoke-class-method condition "reason")
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


(defmethod print-object ((object objc-id) stream)
  (print-unreadable-object (object stream)
    (format stream "~A `~A' {~X}"
            (objcl-class-name
             (objcl-invoke-class-method object "class"))
            (objcl-invoke-class-method
             (objcl-invoke-class-method object "description")
             "UTF8String")
            (objcl-invoke-class-method object "hash"))))


(defmethod print-object ((object objc-class) stream)
  (print-unreadable-object (object stream)
    (format stream "OBJC-CLASS ~A"
            (objcl-class-name object))))


;;;; (@* "Convenience types")
(deftype c-pointer ()
  '(satisfies pointerp))
