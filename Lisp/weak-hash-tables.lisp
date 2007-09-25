;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007  Matthias Andreas Benkard.
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

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
  (declaim (inline make-weak-value-hash-table (setf weak-gethash)))

  (defun make-weak-value-hash-table ()
    (trivial-garbage:make-weak-hash-table :weakness :value
                                          :test 'eql))

  (setf (fdefinition 'weak-gethash) (fdefinition 'gethash))

  (defun (setf weak-gethash) (value key hash-table)
    (setf (gethash key hash-table) value)))
