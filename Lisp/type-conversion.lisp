(in-package #:mulk.objective-cl)


;;; (@* "Data conversion")
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


(defun obj-data->lisp (obj-data)
  (with-foreign-slots ((type data) obj-data obj-data)
    (let* ((type-name (type-id->type-name (foreign-string-to-lisp type)))
           (lisp-type (type-name->lisp-type type-name))
           (value     (if (eq 'void type-name)
                          (values)
                          (foreign-slot-value data
                                              'obj-data-union
                                              (type-name->slot-name type-name)))))
      (case lisp-type
        ((id objc-class selector exception)
         (make-instance lisp-type :pointer value))
        ((string) (foreign-string-to-lisp value))
        (otherwise value)))))


(defmacro with-foreign-conversion (bindings &body body)
  `(with-foreign-objects
       ,(mapcar #'(lambda (name-value-pair)
                     (destructuring-bind (name value)
                         name-value-pair
                       `(,name (lisp->obj-data ,value))))
                bindings)
     ,@body))


(defmacro with-foreign-objects (bindings &body body)
  `(let ,(mapcar #'(lambda (name-value-pair)
                     (destructuring-bind (name value)
                         name-value-pair
                       `(,name ,value)))
                 bindings)
     (unwind-protect
          (progn ,@body)
       ,@(mapcar #'(lambda (name-value-pair)
                     `(dealloc-obj-data ,(first name-value-pair)))
                 bindings))))


(defun foreign-string-to-lisp/dealloc (foreign-string)
  "Convert a (possibly freshly allocated) C string into a Lisp string
and free the C string afterwards."

  (unwind-protect
       (foreign-string-to-lisp foreign-string)
    (foreign-string-free foreign-string)))
