(in-package #:mulk.objective-cl)


(define-foreign-library libobjcl
  (unix "/home/mulk/Dokumente/Projekte/Objective-CL/Objective-C/shared_obj/libobjcl.so"))

(use-foreign-library libobjcl)


(defcfun "objcl_initialise_runtime" :void)
(defcfun "objcl_shutdown_runtime" :void)
(defcfun ("objcl_invoke_instance_method"
          %objcl-invoke-instance-method) obj-data
  (receiver obj-data)
  (method-name :string)
  (argc :int)
  &rest)

(defcfun ("objcl_invoke_class_method"
          %objcl-invoke-class-method) obj-data
  (receiver obj-data)
  (method-name :string)
  (argc :int)
  &rest)

(defcfun ("objcl_find_class" %objcl-find-class) :pointer
  (class-name :string))

(defcfun ("objcl_class_name" %objcl-class-name) :string
  (class obj-data))


(defun objcl-find-class (class-name)
  (let ((obj-data (%objcl-find-class class-name)))
    (prog1
        (if (null-pointer-p (foreign-slot-value
                             (foreign-slot-value obj-data 'obj-data 'data)
                             'obj-data-union
                             'class-val))
            nil
            (obj-data->lisp obj-data))
      (dealloc-obj-data obj-data))))


(defun objcl-class-name (class)
  (declare (type (or objc-class objc-id objc-exception) class))
  (let ((obj-data (foreign-alloc 'obj-data)))
    (with-foreign-slots ((type data) obj-data obj-data)
      (setf (foreign-slot-value obj-data
                                'obj-data-union
                                (etypecase class
                                  (objc-class     'class-val)
                                  (objc-id        'id-val)
                                  (objc-exception 'exc-val)))
            (pointer-to class))
      (setf type (foreign-string-alloc (etypecase class
                                         (objc-class     "#")
                                         (objc-id        "@")
                                         (objc-exception "E")))))
    (prog1
        (%objcl-class-name obj-data)
      (dealloc-obj-data obj-data))))

