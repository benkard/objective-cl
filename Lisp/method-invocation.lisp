(in-package #:mulk.objective-cl)


;;; (@* "Method invocation")
(defun invoke (receiver message-start &rest message-components)
  "FIXME"
  (flet ((message-component->string (symbol)
           (let* ((components (split-sequence #\- (symbol-name symbol)
                                              :remove-empty-subseqs t))
                  (acc-string
                   (reduce #'(lambda (x y) (concatenate 'string x y))
                           (mapcar #'(lambda (x)
                                       (concatenate 'string
                                                    (string (char x 0))
                                                    (string-downcase (subseq x 1))))
                                   (subseq components 1))
                           :initial-value (string-downcase (first components)))))
             (if (eql (find-package '#:keyword)
                      (symbol-package symbol))
                 (concatenate 'string acc-string ":")
                 acc-string))))
    (do* ((components-left (cons message-start message-components)
                           (cddr components-left))
          (message-string (message-component->string message-start)
                          (concatenate 'string
                                       message-string
                                       (message-component->string (first components-left))))
          (arglist        (if (null (rest components-left))
                              nil
                              (list (second components-left)))
                          (if (null (rest components-left))
                              arglist
                              (cons (second components-left) arglist))))
        ((null (cddr components-left))
         (apply #'invoke-by-name receiver message-string (nreverse arglist))))))


(defun invoke-by-name (receiver method-name &rest args)
  "FIXME"
  (let* ((arglist (arglist-intersperse-types
                   (mapcar #'lisp->obj-data args)))
         (return-value (apply-macro '%objcl-invoke-class-method
                                    (lisp->obj-data receiver)
                                    method-name
                                    (length args)
                                    arglist)))
    (when *trace-method-calls*
      (format t "~&Invoking [~A].~%" method-name))
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
(defun invoke-instance-method-by-name (receiver method-name &rest args)
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
