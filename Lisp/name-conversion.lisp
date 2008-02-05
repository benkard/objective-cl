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
  (let ((case-converted-name
         (name-hyphened->mixed-case (symbol-name symbol) :camel-case)))
    (if (eql (find-package '#:keyword)
             (symbol-package symbol))
        (concatenate 'string case-converted-name ":")
        case-converted-name)))


(defun symbol-list->message-name (symbol-list)
  (reduce #'(lambda (acc symbol)
              (concatenate 'string acc (message-component->string symbol)))
          symbol-list
          :initial-value ""))


;;; (@* "Class names")
(defun symbol->objc-class-name (symbol)
  (let* ((name (symbol-name symbol))
         (hyphen-pos (position #\- name))
         (head (if hyphen-pos
                   (subseq name 0 hyphen-pos)
                   nil))
         (tail (if hyphen-pos
                   (subseq name hyphen-pos)
                   name))
         (converted-tail
          (name-hyphened->mixed-case tail :nerd-caps)))
    (if head
        (concatenate 'string
                     (string-upcase head)
                     converted-tail)
        converted-tail)))


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


(defun name-hyphened->mixed-case (string &optional (case-convention :nerd-caps))
  (let ((lower-case-string (name->canonised-lower-case string)))
    (ecase case-convention
      ((:camel-case) (name-hyphened->camel-case lower-case-string))
      ((:nerd-caps) (name-hyphened->nerd-caps lower-case-string))
      ((:underscored) (name-hyphened->underscored lower-case-string))
      ((:hyphened) lower-case-string))))


(defun slot-name->foreign-slot-name (slot-name
                                     &key (case-convention :camel-case))
  (name-hyphened->mixed-case (symbol-name slot-name) case-convention))


(defun foreign-slot-name->slot-name (foreign-slot-name)
  (let ((*package* (find-package '#:objective-c-classes)))
    (export-and-return (read-from-string (name-underscored->hyphened
                                          (name-camel-case->hyphened
                                           foreign-slot-name))))))


(defun name-underscored->hyphened (string)
  (substitute #\- #\_ string))


(defun name-hyphened->underscored (string)
  (substitute #\_ #\- string))


(defun name->canonised-lower-case (string)
  (cond ((name-in-canonical-case-p string) (string-downcase string))
        ((and (eq (readtable-case *readtable*) :invert)
              (notany #'upper-case-p string))
         (string-upcase string))
        (t string)))


(defun name-in-canonical-case-p (string
                                 &optional
                                 (case-mode (readtable-case *readtable*)))
  (or (and (member case-mode '(:downcase :invert :preserve))
           (notany #'upper-case-p string))
      (and (member case-mode '(:upcase))
           (notany #'lower-case-p string))))


(defun string-capitalise-lower-case (string)
  "Like STRING-CAPITALIZE except that all upper-case characters are left alone."
  (with-output-to-string (out)
    (loop for previous-position = 0 then word-start
          for delimiter-pos = (position-if-not #'alphanumericp
                                               string
                                               :start previous-position)
          for word-start = (and delimiter-pos (1+ delimiter-pos))
          do (format out
                     "~:(~C~)~A"
                     (char string previous-position)
                     (subseq string
                             (1+ previous-position)
                             (or word-start (length string))))
          while word-start)))


(defun name-hyphened->camel-case (string)
  (remove #\- (concatenate 'string
                           (string (char string 0))
                           (subseq (string-capitalise-lower-case string) 1))))


(defun name-camel-case->hyphened (string)
  (with-output-to-string (out)
    (loop for previous-position = 0 then word-start
          for word-start = (position-if #'upper-case-p
                                        string
                                        :start (1+ previous-position))
          do (format out "~(~A~)" (subseq string
                                          previous-position
                                          (or word-start (length string))))
          while word-start
          do (format out "-"))))


(defun name-hyphened->nerd-caps (string)
  (remove #\- (string-capitalise-lower-case string)))
