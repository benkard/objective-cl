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


;; FIXME: Document.
(defun define-returns-boolean-exception (selector-designator)
  (let ((key (typecase selector-designator
               (string selector-designator)
               (t (selector-name (selector selector-designator))))))
    (setf (gethash key *boolean-return-exceptions*) t)))


;; FIXME: Document.
(defun undefine-returns-boolean-exceptions (selector-designator)
  (let ((key (typecase selector-designator
               (string selector-designator)
               (t (selector-name (selector selector-designator))))))
    (remhash key *boolean-return-exceptions*)))


(define-returns-boolean-exception "charValue")
(define-returns-boolean-exception "characterAtIndex:")


(defun returned-char-is-bool-p (receiver selector)
  (declare (ignore receiver))
  (gethash (selector-name selector) *boolean-return-exceptions* nil))


(defun objc-char->lisp-value (objc-char char-is-bool-p)
  (if char-is-bool-p
      objc-char
      (not (zerop objc-char))))
