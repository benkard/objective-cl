(in-package #:mulk.objective-cl)


;;; (@* "Method invocation")
(defun objcl-invoke-class-method (receiver method-name &rest args)
  (let* ((arglist (arglist-intersperse-types
                   (mapcar #'lisp->obj-data args)))
         (return-value (apply-macro '%objcl-invoke-class-method
                                    (lisp->obj-data receiver)
                                    method-name
                                    (length args)
                                    arglist)))
    (format t "~&Invoking [~A].~%" method-name)
    (unwind-protect
         (let ((value
                (let ((*skip-retaining* (or *skip-retaining*
                                            (constructor-name-p method-name))))
                  (obj-data->lisp return-value))))
           (if (typep value 'condition)
               (cerror "Return NIL from OBJCL-INVOKE-CLASS-METHOD" value)
               value))
      (dealloc-obj-data return-value))))


#+nil
(defun objcl-invoke-class-method (receiver method-name &rest args)
  (let* ((arglist (arglist-intersperse-types
                   (mapcar #'lisp->obj-data args)))
         (return-value (apply-macro '%objcl-invoke-instance-method
                                    (lisp->obj-data receiver)
                                    method-name
                                    (length args)
                                    arglist)))
    (format t "~&Invoking <~A>.~%" method-name)
    (unwind-protect
         (let ((value
                (let ((*skip-retaining* (or *skip-retaining*
                                            (constructor-name-p method-name))))
                  (obj-data->lisp return-value))))
           (if (typep value 'condition)
               (cerror "Return NIL from OBJCL-INVOKE-INSTANCE-METHOD" value)
               value))
      (dealloc-obj-data return-value))))


;;; (@* "Helper functions")
(defun arglist-intersperse-types (arglist)
  (mapcan #'(lambda (arg)
              (list :pointer arg))
          arglist))


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
