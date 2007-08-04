(in-package #:mulk.objective-cl)


;;; (@* "Method invocation")
(defun invoke (receiver message-start &rest message-components)
  "Send a message to an Objective C instance.

RECEIVER: an Objective C wrapper object.

MESSAGE-START: a symbol.

MESSAGE-COMPONENTS: an alternating list of arguments and message name
component symbols.

Returns: the return value of the method invocation.


Each message name component is first split into parts seperated by
hyphens and each part is converted into a string according to the
following rules:

 1. The first part is fully converted to lower case.

 2. Any additional parts are also fully converted to lower case except
 for their first letters, which are left intact.

 3. If the symbol is a keyword symbol, the resulting string is suffixed
 by a colon (`:').

After that, all parts are concatenated in order to form a single message
component.  The message components are in turn concatenated in order to
form the message name which is used as if the second argument to a call
to INVOKE-BY-NAME.

The message components that are not message name components are
collected in order and the resulting list used as if as additional
arguments to INVOKE-BY-NAME.


Examples:

 (invoke (find-objc-class 'ns-string)
         :string-with-c-string \"Mulk.\")
   ;=> #<GSCBufferString `Mulk.' {5B36087}>

 (invoke (find-objc-class 'ns-object)
         'self)                           
   ;=> #<NSObject `NSObject' {16ECF598}>

 (invoke (find-objc-class 'ns-object)
         'name)                           
   ;=> \"NSObject\"

 (invoke (find-objc-class 'ns-string)
         :string-with-c-string \"Mulk.\"
         :encoding 4)
   ;=> #<GSCBufferString `Mulk.' {5B36087}>


See also: INVOKE-BY-NAME"

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
  "Send a message to an Objective C object by the name of the method.

RECEIVER: an Objective C wrapper object.

METHOD-NAME: a string.

ARGS: a list of objects.

Returns: the return value of the method invocation.


Examples:

 (invoke-by-name (find-objc-class 'ns-string)
                 \"stringWithCString:\" \"Mulk.\")
   ;=> #<GSCBufferString `Mulk.' {5B36087}>

 (invoke-by-name (find-objc-class 'ns-object)
                 \"self\")
   ;=> #<NSObject `NSObject' {16ECF598}>

 (invoke-by-name (find-objc-class 'ns-string)
                 \"stringWithCString:encoding:\"
                 \"Mulk.\"
                 4)
   ;=> #<GSCBufferString `Mulk.' {5B36087}>


See also: INVOKE"

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
