(in-package #:mulk.objective-cl)


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
                       (unless *skip-retaining*
                         (invoke-by-name new-obj "retain"))
                       (unless *skip-finalization*
                         ;; We only put the new object into the hash
                         ;; table if it is a regular wrapper object
                         ;; rather than a temporary one, else the object
                         ;; pointed to might be released prematurely
                         ;; because of the lack of memory management.
                         (setf (weak-gethash hash-key hash-table) new-obj)
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
                                      (invoke-by-name temp "release"))))
                             (trivial-garbage:finalize new-obj #'finalizer))))
                       new-obj))
               (t obj))))

         (defmethod initialize-instance ((obj ,type) &key)
           (call-next-method)))))))
