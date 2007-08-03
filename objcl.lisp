(in-package #:mulk.objective-cl)

(define-foreign-library libobjcl
  (unix "/home/mulk/Dokumente/Projekte/Objective-CL/shared_obj/libobjcl.so"))

(use-foreign-library libobjcl)

(defctype pointer-array :pointer
  "An array of void pointers.")


(deftype c-pointer ()
  '(satisfies pointerp))


(defclass c-pointer-wrapper ()
  ((pointer :type     c-pointer
            :accessor pointer-to
            :initarg  :pointer)))


(defclass objc-selector (c-pointer-wrapper) ())
(defclass objc-id       (c-pointer-wrapper) ())
(defclass objc-class    (c-pointer-wrapper) ())


(defcunion obj-data-union
  (id-val :pointer)
  (class-val :pointer)
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
  (type :string)
  (data obj-data-union))


(defun dealloc-obj-data (obj-data)
  #+nil
  (with-foreign-slots ((type data) obj-data obj-data)
    (free-translated-object type :string '(t)))
  (foreign-free obj-data))


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


;;; Copied from objc-api.h
;;; Probably ought to be generated by C code at initialisation time.
(defparameter *objcl-api-type-names*
  '((id       . #\@)
    (class    . #\#)
    (sel      . #\:)
    (chr      . #\c)
    (uchr     . #\C)
    (sht      . #\s)
    (usht     . #\S)
    (int      . #\i)
    (uint     . #\I)
    (lng      . #\l)
    (ulng     . #\L)
    (lng-lng  . #\q)
    (ulng-lng . #\Q)
    (flt      . #\f)
    (dbl      . #\d)
    (bfld     . #\b)
    (bool     . #\B)
    (void     . #\v)
    (undef    . #\?)
    (ptr      . #\^)
    (charptr  . #\*)
    (atom     . #\%)
    (ary-b    . #\[)
    (ary-e    . #\])
    (union-b  . #\()
    (union-e  . #\))
    (struct-b . #\{)
    (struct-e . #\})
    (vector   . #\!)
    (complex  . #\j)))


(defparameter *objcl-data-map*
  '((id       . id-val)
    (class    . class-val)
    (sel      . sel-val)
    (chr      . char-val)
    (uchr     . char-val)
    (sht      . short-val)
    (usht     . short-val)
    (int      . int-val)
    (uint     . int-val)
    (lng      . long-val)
    (ulng     . long-val)
    (lng-lng  . long-long-val)
    (ulng-lng . long-long-val)
    (flt      . float-val)
    (dbl      . double-val)
    (bool     . bool-val)
    (ptr      . ptr-val)
    (charptr  . charptr-val)))


(defparameter *objcl-type-map*
  '((id       . objc-id)
    (class    . objc-class)
    (sel      . objc-selector)
    (chr      . character)
    (int      . integer)
    (uint     . integer)
    (lng      . integer)
    (ulng     . integer)
    (sht      . integer)
    (usht     . integer)
    (lng-lng  . integer)
    (ulng-lng . integer)
    (flt      . single-float)
    (dbl      . double-float)
    (bool     . boolean)
    (ptr      . c-pointer)
    (charptr  . string)))

(defparameter *objcl-c-type-map*
  '((id       . :pointer)
    (class    . :pointer)
    (sel      . :pointer)
    (chr      . :char)
    (int      . :int)
    (uint     . :unsigned-int)
    (lng      . :long)
    (ulng     . :unsigned-long)
    (sht      . :short)
    (usht     . :unsigned-short)
    (lng-lng  . :long-long)
    (ulng-lng . :unsigned-long-long)
    (flt      . :float)
    (dbl      . :double)
    (bool     . :boolean)
    (ptr      . :pointer)
    (charptr  . :pointer)))


(defun apply-macro (macro-name arg &rest args)
  "Because FOREIGN-FUNCALL is a macro.  Why, oh why is this?"
  (funcall
   (compile nil
            `(lambda ()
               (,macro-name ,@(butlast (cons arg args))
                            ,@(car (last (cons arg args))))))))


(defun lisp-value->type-name (value)
  (car (rassoc-if #'(lambda (type)
                      (typep value type))
                  *objcl-type-map*)))

(defun type-name->lisp-type (type-name)
  (cdr (assoc type-name *objcl-type-map*)))

(defun type-name->slot-name (type-name)
  (cdr (assoc type-name *objcl-data-map*)))

(defun type-name->type-id (type-name)
  (string (cdr (assoc type-name *objcl-api-type-names*))))

(defun type-id->type-name (type-id)
  (car (rassoc (char type-id 0) *objcl-api-type-names*)))

(defun type-name->c-type (type-name)
  (cdr (assoc type-name *objcl-c-type-map*)))

(defun arglist-intersperse-types (arglist)
  (mapcan #'(lambda (arg)
              (with-foreign-slots ((type data) arg obj-data)
                (list (type-name->c-type (type-id->type-name type))
                      arg)))
          arglist))


(defun objcl-invoke-instance-method (receiver method-name &rest args)
  (let* ((arglist (arglist-intersperse-types
                   (mapcar #'lisp->obj-data args)))
         (return-value (apply-macro '%objcl-invoke-instance-method
                                    (lisp->obj-data receiver)
                                    method-name
                                    (length args)
                                    arglist)))
    (prog1
        (obj-data->lisp return-value)
      (dealloc-obj-data return-value))))


(defun objcl-invoke-class-method (class method-name &rest args)
  (let* ((arglist (arglist-intersperse-types
                   (mapcar #'lisp->obj-data args)))
         (return-value (apply-macro '%objcl-invoke-class-method
                                    (lisp->obj-data class)
                                    method-name
                                    (length args)
                                    arglist)))
    (prog1
        (obj-data->lisp return-value)
      (dealloc-obj-data return-value))))


(defun lisp->obj-data (value)
  (let ((obj-data (foreign-alloc 'obj-data))
        (type-name (lisp-value->type-name value)))
    (with-foreign-slots ((type data) obj-data obj-data)
      (setf (foreign-slot-value data
                                'obj-data-union
                                (type-name->slot-name type-name))
            (typecase value
              ((or objc-id objc-class objc-selector)
               (pointer-to value))
              (otherwise value)))
      (setf type
            (type-name->type-id type-name)))
    obj-data))


(defun obj-data->lisp (obj-data)
  (with-foreign-slots ((type data) obj-data obj-data)
    (let* ((type-name (type-id->type-name type))
           (lisp-type (type-name->lisp-type type-name))
           (value     (foreign-slot-value data
                                          'obj-data-union
                                          (type-name->slot-name type-name))))
      (case lisp-type
        ((objc-id objc-class objc-selector)
         (make-instance lisp-type :pointer value))
        (otherwise value)))))


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
  (declare (type (or objc-class objc-id) class))
  (let ((obj-data (foreign-alloc 'obj-data)))
    (with-foreign-slots ((type data) obj-data obj-data)
      (setf type (typecase class
                   (objc-class "#")
                   (objc-id    "@")))
      (setf (foreign-slot-value obj-data
                                'obj-data-union
                                (typecase class
                                  (objc-class 'class-val)
                                  (objc-id    'id-val)))
            (pointer-to class)))
    (prog1
        (%objcl-class-name (pointer-to class))
      (dealloc-obj-data obj-data))))


(defmethod print-object ((object objc-id) stream)
  (print-unreadable-object (object stream)
    (format stream "~A OBJC-ID"
            (objcl-class-name
             (objcl-invoke-instance-method object "class")))))


(defmethod print-object ((object objc-class) stream)
  (print-unreadable-object (object stream)
    (format stream "OBJC-CLASS ~A"
            (objcl-class-name object))))


(set-macro-character #\] (get-macro-character #\)))

(set-macro-character #\[ #'(lambda (stream char)
                             (declare (ignore char))
                             (parse-objc-call stream)))

(defun parse-objc-call (stream)
  (let ((*standard-input* stream))
    (flet ((read-message-part (buffer)
             (do ((char (read-char stream t nil t)
                        (read-char stream t nil t)))
                 ((not (or (alphanumericp char)
                           (member char (coerce ":_-" 'list))))
                  (unread-char char))
               (vector-push-extend char buffer)))
           (slurp-whitespace ()
             (do ((char nil
                        (read-char stream t nil t)))
                 ((not (member (peek-char) '(#\Space #\Newline #\Tab)))))))
      (let* ((class-method-p nil)
             (receiver (if (upper-case-p (peek-char))
                           ;; A class name.
                           (let ((*readtable* (copy-readtable)))
                             (setf class-method-p t)
                             (setf (readtable-case *readtable*) :preserve)
                             `(objcl-find-class
                               ,(symbol-name (read stream t nil t))))
                           ;; Something else.
                           (read stream t nil t)))
             (args (list))
             (message (make-array '(0) :element-type 'character
                                       :adjustable t :fill-pointer t)))

        (slurp-whitespace)
        (do ()
            ((char= #\] (peek-char)))
          (read-message-part message)
          (slurp-whitespace)
          (unless (char= #\] (peek-char))
            (push (read stream t nil t) args)
            (slurp-whitespace)))

        ;; Slurp the trailing #\].
        (assert (char= #\] (read-char)))
        (setf args (nreverse args))
        `(,(if class-method-p
               'objcl-invoke-class-method
               'objcl-invoke-instance-method)
           ,receiver
           ,(make-array (list (length message))
                        :element-type 'character
                        :initial-contents message
                        :adjustable nil
                        :fill-pointer nil)
           ,@args)))))
