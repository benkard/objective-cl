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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (find-objc-class "NSObject" t))


(defclass ns::mlk-lisp-value (ns::ns-object)
     ((lisp-value :initarg :value
                  :initform nil
                  :accessor lisp-value))
  (:metaclass ns::+ns-object))


(defun make-lisp-value (value)
  ;; FIXME: The following won't work.  Make MAKE-INSTANCE more useful...
  ;(make-instance 'ns::mlk-lisp-value :value value)
  (let ((instance (invoke (find-class 'ns::mlk-lisp-value) 'new)))
    (setf (lisp-value instance) value)
    instance))
