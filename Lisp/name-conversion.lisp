;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007  Matthias Andreas Benkard.
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public License
;;;; as published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(in-package #:mulk.objective-cl)


;;; (@* "Message and selector names")
(defun message-component->string (symbol)
  (let* ((components (split-sequence #\- (symbol-name symbol)
                                     :remove-empty-subseqs t))
         (downcasep (notany #'lower-case-p (symbol-name symbol)))
         (acc-string
          (reduce #'(lambda (x y) (concatenate 'string x y))
                  (mapcar #'(lambda (x)
                              (if downcasep
                                  (concatenate 'string
                                               (string (char x 0))
                                               (string-downcase (subseq x 1)))
                                  x))
                          (subseq components 1))
                  :initial-value (if downcasep
                                     (string-downcase (first components))
                                     (first components)))))
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


(defun export-and-return (symbol)
  (export (list symbol))
  symbol)


(defun objc-class-name->symbol (class-name)
  (let ((prefix-end (1- (position-if #'lower-case-p class-name))))
    (cond ((and prefix-end (> prefix-end 0))
           ;; There are n upper case chars at the head of the name.
           ;; Take the first (1- n) of them and downcase them.  Then,
           ;; put a dash right after them and downcase the n'th char as
           ;; well, such that "NSFoo" becomes "ns-foo".
           (setq class-name (concatenate 'string
                                         (string-downcase
                                          (subseq class-name 0 prefix-end))
                                         "-"
                                         (string
                                          (char-downcase
                                           (char class-name prefix-end)))
                                         (subseq class-name (1+ prefix-end)))))
          ((and prefix-end (zerop prefix-end))
           ;; There is exactly one upper case char at the head of the
           ;; name.  just downcase it and move on.
           (setq class-name (copy-seq class-name))
           (setf (char class-name 0) (char-downcase (char class-name 0))))))
  (loop for delim-position = (position-if #'upper-case-p class-name)
        while delim-position
        do (setq class-name (concatenate 'string
                                         (subseq class-name 0 delim-position)
                                         "-"
                                         (string
                                          (char-downcase
                                           (char class-name delim-position)))
                                         (subseq class-name (1+ delim-position)))))
  (let ((*package* (find-package '#:objective-c-classes)))
    ;; Why do we use READ-FROM-STRING rather than MAKE-SYMBOL?  That is
    ;; because we want this procedure to work as expected for any value
    ;; of (READTABLE-CASE *READTABLE*), which means that 'ns-object
    ;; should always mean the same thing as "NSObject".
    (export-and-return
     (read-from-string class-name))))


(defun objc-meta-class-name->symbol (meta-class-name)
  (let ((*package* (find-package '#:objective-c-classes)))
    (export-and-return
     (read-from-string
      (concatenate 'string
                   "+"
                   (symbol-name (objc-class-name->symbol meta-class-name)))))))


(defun slot-name->foreign-slot-name (slot-name)
  (substitute #\_ #\- (name->lower-case (symbol-name slot-name))))


(defun name->lower-case (string)
  (cond ((name-typed-in-canonical-case-p) (string-downcase string))
        ((and (eq (readtable-case *readtable*) :invert)
              (notany #'upper-case-p string))
         (string-upcase string))
        (t string)))


(defun name-typed-in-canonical-case-p (string)
  (or (and (member (readtable-case *readtable*)
                   '(:downcase :invert :preserve))
           (notany #'upper-case-p string))
      (and (member (readtable-case *readtable*))
           (notany #'lower-case-p string))))
