(in-package #:mulk.objective-cl)

(define-foreign-library libobjcl
  (unix "/home/mulk/Dokumente/Projekte/Objective-CL/shared_obj/libobjcl.so"))

(use-foreign-library libobjcl)

(defctype pointer-array :pointer
  "An array of void pointers.")

#+nil
(defmethod translate-to-foreign ((vector vector)
                                 (type (eql 'pointer-array)))
  (foreign-alloc :pointer
                 :count (length vector)
                 :null-terminated-p nil
                 :initial-contents vector))

#+nil
(defmethod translate-from-foreign (foreign-value
                                   (type (eql 'pointer-array)))
  )


(defcfun "objcl_initialise_runtime" :void)
(defcfun "objcl_shutdown_runtime" :void)
(defcfun "objcl_invoke_instance_method" :pointer
  (receiver :pointer)
  (method-name :string)
  (argc :int)
  &rest)

(defcfun "objcl_invoke_class_method" :pointer
  (receiver :void)
  (method-name :string)
  (argc :int)
  &rest)

(defcfun "objcl_find_class" :pointer
  (class-name :string))
