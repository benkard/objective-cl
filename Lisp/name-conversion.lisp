(in-package #:mulk.objective-cl)


;;; (@* "Message and selector names")
(defun message-component->string (symbol)
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
        acc-string)))


(defun symbol-list->message-name (symbol-list)
  (reduce #'(lambda (acc symbol)
              (concatenate 'string acc (message-component->string symbol)))
          symbol-list
          :initial-value ""))


;;; (@* "Class names")
(defun symbol->objc-class-name (symbol)
  (let ((components (split-sequence #\- (symbol-name symbol)
                                    :remove-empty-subseqs t)))
    (reduce #'(lambda (x y) (concatenate 'string x y))
            (mapcar #'(lambda (x)
                        (concatenate 'string
                                     (string (char x 0))
                                     (string-downcase (subseq x 1))))
                    (subseq components 1))
            :initial-value (concatenate 'string
                                        (string (char (first components) 0))
                                        (string-upcase
                                         (subseq (first components) 1))))))
