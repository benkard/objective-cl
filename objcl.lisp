(in-package #:mulk.objective-cl)

(define-foreign-library libobjcl
  (unix "/home/mulk/Dokumente/Projekte/Objective-CL/shared_obj/libobjcl.so"))

(use-foreign-library libobjcl)


(deftype c-pointer ()
  '(satisfies pointerp))


(defctype char-pointer :pointer)

(defmethod translate-to-foreign ((value string) (type (eql 'char-pointer)))
  #+nil
  (let ((buffer (foreign-alloc :char :count (length value))))
    (cffi:lisp-string-to-foreign value buffer (length value))
    buffer)
  (foreign-string-alloc value))

(defmethod translate-from-foreign (c-value (type (eql 'char-pointer)))
  (foreign-string-to-lisp c-value))


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


#+cmu
(progn
  (declaim (inline make-weak-value-hash-table))

  (defun make-weak-value-hash-table ()
    (make-hash-table :test 'eql))

  (defun weak-gethash (key hash-table &optional (default nil))
    (let ((pointer (gethash key hash-table default)))
      (or (and (trivial-garbage:weak-pointer-p pointer)
               (trivial-garbage:weak-pointer-value pointer))
          (prog1 default
            ;; Clean up.
            (remhash key hash-table)))))

  (defun (setf weak-gethash) (value key hash-table)
    (setf (gethash key hash-table)
          (trivial-garbage:make-weak-pointer value))))

#-cmu
(progn
  (declaim (inline make-weak-value-hash-table))

  (defun make-weak-value-hash-table ()
    (trivial-garbage:make-weak-hash-table :weakness :value
                                          :test 'eql))

  (setf (fdefinition 'weak-gethash)        (fdefinition 'gethash)
        (fdefinition '(setf weak-gethash)) (fdefinition '(setf gethash))))


(defvar *skip-finalization* nil)
(defvar *skip-retaining*    nil)

(defvar *id-objects* (make-weak-value-hash-table))
(defvar *class-objects* (make-weak-value-hash-table))
(defvar *exception-objects* (make-weak-value-hash-table))

;; We call the `retain' method on every object that we receive from a
;; method call or otherwise except non-convenience constructor methods
;; (i.e. those whose name starts with `alloc' or `new').  Upon
;; Lisp-side finalization of an object, wie `release' it.
(eval-when (:load-toplevel)
  (dolist (type '(objc-id objc-class objc-exception))
    (funcall
     (compile
      nil
      `(lambda ()
         (defmethod make-instance ((class (eql ',type)) &rest initargs &key)
           (let* ((hash-table ,(ecase type
                                 ((objc-id)        '*id-objects*)
                                 ((objc-class)     '*class-objects*)
                                 ((objc-exception) '*exception-objects*)))
                  (hash-key (pointer-address (getf initargs :pointer)))
                  (obj (weak-gethash hash-key hash-table nil)))
             (typecase obj
               (keyword (assert (eq :incomplete obj))
                        (call-next-method))
               (null (setf (weak-gethash hash-key hash-table)
                           :incomplete)
                     (let ((new-obj (call-next-method)))
                       (setf (weak-gethash hash-key hash-table) new-obj)
                       (unless *skip-retaining*
                         (objcl-invoke-class-method new-obj "retain"))
                       (unless *skip-finalization*
                         (assert (not (null (pointer-to new-obj))))
                         (let ((saved-pointer (pointer-to new-obj))
                               (saved-type    (type-of new-obj)))
                           (flet ((finalizer ()
                                    ;; In order to send the `release'
                                    ;; message to the newly GC'd object,
                                    ;; we have to create a temporary
                                    ;; container object for the final
                                    ;; message delivery.  Note that this
                                    ;; can cause an infinite recursion
                                    ;; or even memory corruption if we
                                    ;; don't take measure to skip both
                                    ;; finalization and retaining of the
                                    ;; temporary object.
                                    (let ((temp (let ((*skip-finalization* t)
                                                      (*skip-retaining*    t))
                                                  (make-instance saved-type
                                                                 :pointer saved-pointer))))
                                      (objcl-invoke-class-method temp "release"))))
                             (trivial-garbage:finalize new-obj #'finalizer))))
                       new-obj))
               (t obj))))

         (defmethod initialize-instance ((obj ,type) &key)
           (call-next-method)))))))


(defgeneric objcl-eql (obj1 obj2))
(defmethod objcl-eql ((obj1 c-pointer-wrapper) (obj2 c-pointer-wrapper))
  (pointer-eq (pointer-to obj1) (pointer-to obj2)))
(defmethod objcl-eql (obj1 obj2)
  (eql obj1 obj2))


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


(defun dealloc-obj-data (obj-data)
  (with-foreign-slots ((type data) obj-data obj-data)
    (foreign-string-free type))
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
    (exc      . #\E)
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
    (exc      . exc-val)
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
    (exc      . objc-exception)
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
    (exc      . :pointer)
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
              (list :pointer arg))
          arglist))


#+nil
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


(defun constructor-name-p (method-name)
  (flet ((method-name-starts-with (prefix)
           (and (>= (length method-name) (length prefix))
                (or (and (string= prefix
                                  (subseq method-name 0 (length prefix)))
                         (or (= (length method-name)
                                (length prefix))
                             (not (lower-case-p (char method-name (length prefix))))))))))
    (or (method-name-starts-with "alloc")
        (method-name-starts-with "new"))))


(defun objcl-invoke-class-method (class method-name &rest args)
  (let* ((arglist (arglist-intersperse-types
                   (mapcar #'lisp->obj-data args)))
         (return-value (apply-macro '%objcl-invoke-class-method
                                    (lisp->obj-data class)
                                    method-name
                                    (length args)
                                    arglist)))
    (unwind-protect
         (let ((value
                (let ((*skip-retaining* (or *skip-retaining*
                                            (constructor-name-p method-name))))
                  (obj-data->lisp return-value))))
           (if (typep value 'condition)
               (cerror "Return NIL from OBJCL-INVOKE-CLASS-METHOD" value)
               value))
      (dealloc-obj-data return-value))))


(defun lisp->obj-data (value)
  (let ((obj-data (foreign-alloc 'obj-data))
        (type-name (lisp-value->type-name value)))
    (with-foreign-slots ((type data) obj-data obj-data)
      (setf (foreign-slot-value data
                                'obj-data-union
                                (type-name->slot-name type-name))
            (typecase value
              ((or objc-id objc-class objc-selector objc-exception)
               (pointer-to value))
              (string    (foreign-string-alloc value))
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
        ((objc-id objc-class objc-selector objc-exception)
         (make-instance lisp-type :pointer value))
        ((string)  (foreign-string-to-lisp value))
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
               #+nil 'objcl-invoke-instance-method
               #-nil 'objcl-invoke-class-method)
           ,receiver
           ,(make-array (list (length message))
                        :element-type 'character
                        :initial-contents message
                        :adjustable nil
                        :fill-pointer nil)
           ,@args)))))
