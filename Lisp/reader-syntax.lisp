(in-package #:mulk.objective-cl)


(defun install-reader-syntax ()
  (set-macro-character #\] (get-macro-character #\)))

  (set-macro-character #\[ #'(lambda (stream char)
                               (declare (ignore char))
                               (parse-objc-call stream))))


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
                             `(find-objc-class
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
               'invoke-by-name
               #+nil 'objcl-invoke-instance-method
               #-nil 'invoke-by-name)
           ,receiver
           ,(make-array (list (length message))
                        :element-type 'character
                        :initial-contents message
                        :adjustable nil
                        :fill-pointer nil)
           ,@args)))))
