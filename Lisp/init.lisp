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

(initialise-runtime)

(eval-when (:load-toplevel)
  (unless (boundp '+nil+)
    ;; As nil is never deallocated, we can safely use MAKE-INSTANCE
    ;; here.
    (defconstant +nil+
      (make-instance 'id :pointer (objcl-get-nil))))
  (unless (boundp '+yes+)
    (defconstant +yes+ (objcl-get-yes)))
  (unless (boundp '+no+)
    (defconstant +no+ (objcl-get-no)))
  (unless (boundp '+runtime-type+)
    (defconstant +runtime-type+ (runtime-type)))
  (pushnew (case +runtime-type+
             ((:gnu) 'objcl-features:gnu-runtime)
             ((:next) 'objcl-features:next-runtime))
           *features*))
