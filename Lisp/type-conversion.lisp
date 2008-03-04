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


(defun convert-from-foreign-value (foreign-value-cell typespec
                                   skip-retaining-p char-is-bool-p)
  (let ((c-type (typespec->c-type typespec)))
    (case (or (typespec-nominal-type typespec)
              (typespec-primary-type typespec))
      ((id objective-c-class exception selector)
       (let ((*skip-retaining* skip-retaining-p))
         (intern-pointer-wrapper (car typespec)
                                 :pointer (cffi:mem-ref foreign-value-cell
                                                        c-type))))
      ((:char :unsigned-char)
       ;; FIXME?  This is non-trivial.  See policy.lisp for
       ;; details.
       (objc-char->lisp-value (cffi:mem-ref foreign-value-cell c-type)
                              char-is-bool-p))
      ((struct union)
       ;; The caller is responsible for preventing the return
       ;; value from being garbage-collected by setting
       ;; FOREIGN-VALUE-LISP-MANAGED-P to false.
       (make-struct-wrapper foreign-value-cell typespec t))
      ((:void) (values))
      (otherwise (cffi:mem-ref foreign-value-cell c-type)))))