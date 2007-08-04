(in-package #:mulk.objective-cl)


(define-foreign-library libobjcl
  (unix "/home/mulk/Dokumente/Projekte/Objective-CL/Objective-C/shared_obj/libobjcl.so"))

(use-foreign-library libobjcl)


;; FIXME: docs
(defcfun ("objcl_initialise_runtime" initialise-runtime) :void)

;; FIXME: docs
(defcfun ("objcl_shutdown_runtime" shutdown-runtime) :void)

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


(defun symbol->objc-class-name (symbol)
  (let ((components (split-sequence #\- (symbol-name symbol)
                                    :remove-empty-subseqs t)))
    (reduce #'(lambda (x y) (concatenate 'string x y))
            (mapcar #'(lambda (x)
                        (concatenate 'string
                                     (string (char x 0))
                                     (string-downcase (subseq x 1))))
                    (subseq components 1))
            :initial-value (concatenate 'string
                                        (string (char (first components) 0))
                                        (string-upcase
                                         (subseq (first components) 1))))))


(defun find-objc-class (class-name)
  "Retrieve an Objective C class by name.

CLASS-NAME: a symbol or a string.

Returns: an OBJC-CLASS object representing the class whose name is
CLASS-NAME.


If CLASS-NAME is a symbol which does not contain a hyphen, its symbol
name is converted to lower case except for the first letter, which is
left intact, and the resulting string used as if directly given as an
argument to FIND-OBJC-CLASS.

If CLASS-NAME is a symbol which containts a hyphen, its symbol name is
split into components seperated by hyphens and each component is
converted into a string according to the following rules:

 1. The first component is fully converted to upper case except for its
 first letter, which is left intact.

 2. Any additional components have all of their letters converted to
 lower case, except for their first letters, which are left intact.

After that, the components are concatenated in order and the resulting
string used as if directly given as an argument to FIND-OBJC-CLASS.


Examples:

 (find-objc-class \"NSObject\")   ;=> #<OBJC-CLASS NSObject>
 (find-objc-class 'ns-object)     ;=> #<OBJC-CLASS NSObject>
 (find-objc-class 'nsobject)      ;=> NIL


Rationale:

The first component of an Objective C class name is conventionally
thought of as a namespace identifier.  It is therefore sensible to
expect it to be converted to upper case by default, which is the
conventional case for namespace identifiers in Objective C."

  (typecase class-name
    (string (find-objc-class-by-name class-name))
    (symbol (find-objc-class-by-name (symbol->objc-class-name class-name)))))


(defun find-objc-class-by-name (class-name)
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

