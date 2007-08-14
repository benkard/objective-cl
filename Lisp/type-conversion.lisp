(in-package #:mulk.objective-cl)


;;; (@* "Low-level Data Conversion")
(declaim (ftype (function (*)
                          (values foreign-pointer &rest nil))
                obj-data->lisp))
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


(declaim (ftype (function (foreign-pointer)
                          (values (or number string symbol selector id
                                      objc-class boolean foreign-pointer)
                                  &rest nil))
                obj-data->lisp))
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


(declaim (ftype (function (foreign-pointer) (values string &rest nil))
                foreign-string-to-lisp/dealloc))
(defun foreign-string-to-lisp/dealloc (foreign-string)
  "Convert a (possibly freshly allocated) C string into a Lisp string
and free the C string afterwards."

  (unwind-protect
       (foreign-string-to-lisp foreign-string)
    (foreign-string-free foreign-string)))


;;; (@* "High-level Data Conversion")
(eval-when (:compile-toplevel :load-toplevel)
  ;; In order to be able to dispatch over pointer types, we need to
  ;; define an alias of the implementation's own pointer class.  Note
  ;; that this may be T (in GNU CLISP, for example), so it's a good idea
  ;; to use CHECK-TYPE in the method body.
  (unless (find-class 'foreign-pointer nil)
    (setf (find-class 'foreign-pointer nil)
          (class-of (make-pointer 0))))
  (deftype foreign-pointer ()
    '(satisfies cffi:pointerp)))


(defun objc-typep (x class-designator)
  (objc-eql (invoke x 'class)
            (etypecase x
              (class x)
              (id (invoke x 'class))
              ((or string symbol) (find-objc-class class-designator t)))))


(defgeneric ->id (x))
(defgeneric ->class (x))
(defgeneric ->integer (x))
(defgeneric ->selector (x))
(defgeneric ->exception (x))
(defgeneric ->character (x))
(defgeneric ->float (x))
(defgeneric ->double (x))
(defgeneric ->bool (x))
(defgeneric ->string (x))
(defgeneric ->pointer (x))


(defmethod ->id ((x id))
  x)

(defmethod ->id ((x class))
  (invoke x 'self))

(defmethod ->id ((x exception))
  (invoke x 'self))

(defmethod ->id ((x integer))
  (let ((id (invoke (find-class 'ns-number)
                    :number-with-long x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))

(defmethod ->id ((x float))
  (let ((id (invoke (find-class 'ns-number)
                    :number-with-double x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))

(defmethod ->id ((x string))
  (let ((id (invoke (find-class 'ns-string)
                    :string-with-c-string x)))
    (invoke id 'retain)
    (invoke id 'autorelease)
    id))


(defmethod ->class ((x id))
  (invoke x 'class))

(defmethod ->class ((x exception))
  (invoke x 'class))

(defmethod ->class ((x class))
  x)

(defmethod ->class ((x string))
  (find-objc-class x t))

(defmethod ->class ((x symbol))
  (find-objc-class x t))


(defmethod ->integer ((x id))
  (assert (objc-typep x 'ns-number))
  (invoke x 'long-value))

(defmethod ->integer ((x number))
  (truncate x))

(defmethod ->integer ((x null))
  0)

(defmethod ->integer ((x symbol))
  (assert (eq 't x))
  1)


(defmethod ->selector ((x selector))
  x)

(defmethod ->selector ((x symbol))
  (selector x))

(defmethod ->selector ((x string))
  (selector x))

(defmethod ->selector ((x cons))
  (selector x))


(defmethod ->exception ((x exception))
  x)


(defmethod ->character ((x character))
  x)

(defmethod ->character ((x integer))
  x)


(defmethod ->float ((x number))
  (float x))


(defmethod ->double ((x number))
  (float x))


(defmethod ->bool ((x null))
  x)

(defmethod ->bool ((x symbol))
  (assert (eq 't x))
  x)

(defmethod ->bool ((x integer))
  x)


(defmethod ->string ((x string))
  x)

(defmethod ->string ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)


(defmethod ->pointer ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)

(defmethod ->pointer ((x exception))
  (pointer-to x))

(defmethod ->pointer ((x c-pointer-wrapper))
  (pointer-to x))

(defmethod ->pointer ((x number))
  (pointer-to (->id x)))
