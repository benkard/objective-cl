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
