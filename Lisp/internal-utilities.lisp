(in-package #:mulk.objective-cl)


(defmacro atomically (&body body)
  ;; Use a reentrant global lock here.
  `(progn ,@body))


(declaim (ftype (function (symbol * &rest *))))
(defun apply-macro (macro-name arg &rest args)
  "Because FOREIGN-FUNCALL is a macro.  Why, oh why is this?"
  (funcall
   (compile nil
            `(lambda ()
               (,macro-name ,@(butlast (cons arg args))
                            ,@(car (last (cons arg args))))))))


(defmacro with-foreign-conversion (bindings &body body)
  `(with-obj-data-values
       ,(mapcar #'(lambda (name-value-pair)
                     (destructuring-bind (name value)
                         name-value-pair
                       `(,name (lisp->obj-data ,value))))
                bindings)
     ,@body))


(defmacro with-obj-data-values (bindings &body body)
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


(defmacro with-foreign-string-pool ((register-fn-name) &body body)
  (let ((pool-var (gensym)))
    `(let ((,pool-var (list)))
       (flet ((,register-fn-name (x)
                (push x ,pool-var)
                x))
         (unwind-protect
             (progn ,@body)
           (dolist (x ,pool-var)
             (cffi:foreign-string-free x)))))))


(defmacro with-foreign-object-pool ((register-fn-name) &body body)
  (let ((pool-var (gensym)))
    `(let ((,pool-var (list)))
       (flet ((,register-fn-name (x)
                (push x ,pool-var)
                x))
         (unwind-protect
             (progn ,@body)
           (dolist (x ,pool-var)
             (cffi:foreign-free x)))))))


(defmacro defcoercion (to-class (object) &body body)
  (let ((type-sym (gensym)))
    `(defmethod coerce-object (,object (,type-sym (eql ',to-class)))
       ,@body)))


;; Compatibility with older versions of CFFI.
(unless (fboundp 'foreign-funcall-pointer)
  (defmacro foreign-funcall-pointer (pointer options &rest args)
    (if (find-symbol "FOREIGN-FUNCALL-POINTER" '#:cffi)
        `(cffi:foreign-funcall-pointer ,pointer ,options ,@args)
        `(cffi:foreign-funcall (,pointer ,@options) ,@args))))
