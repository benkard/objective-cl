;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007, 2008  Matthias Andreas Benkard.
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


(defvar *lisp-managed-instances* (make-hash-table :test 'eql))

(defun intern-lisp-managed-foreign-instance (&rest initargs &key pointer)
  (let ((key (cffi:pointer-address pointer)))
    (with-exclusive-access (*lisp-managed-instances*)
      (or (gethash key *lisp-managed-instances* nil)
          (setf (gethash key *lisp-managed-instances*)
                (apply #'make-instance
                       (intern-pointer-wrapper
                        'class
                        :pointer (%objcl-object-get-class pointer))
                       initargs))))))

(defun unintern-lisp-managed-foreign-instance (instance)
  (with-exclusive-access (*lisp-managed-instances*)
    (remhash (cffi:pointer-address (pointer-to instance))
             *lisp-managed-instances*)))
