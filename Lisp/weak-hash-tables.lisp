(in-package #:mulk.objective-cl)


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
