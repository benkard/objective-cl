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

;;; This file is for hacks that we might not want to actually use in a
;;; production environment but which might be useful in determining
;;; performance bottlenecks.  These hacks may depend on specific
;;; versions of third-party libraries such as CFFI.


;;; The following hack depends on a specific CFFI snapshot.  It tries to
;;; alleviate the apparent slowness of CFFI::PARSE-TYPE in that
;;; particular snapshot.
;;;
;;; The performance improvement for method calls accomplished by this
;;; hack amounts to approximately 80 % for PRIMITIVE-INVOKE and
;;; approximately 50 % for INVOKE.
#+(or)
(progn
  (defparameter *cffi-hacked* nil)
  (eval-when (:load-toplevel)
    ;; If we do this more than once, we cache our own cached function,
    ;; which is kind of useless.
    (unless *cffi-hacked*
      (setq *cffi-hacked* t)
      (let ((original-cffi-parse-type-fn (fdefinition 'cffi::parse-type)))
        (define-cached-function cffi::parse-type (type)
            type
          (funcall original-cffi-parse-type-fn type))))))
