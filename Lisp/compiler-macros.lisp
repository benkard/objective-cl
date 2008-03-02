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


(defun selector-load-time-form (method-name)
  `(load-time-value (handler-case
                        (find-selector ',method-name)
                      (serious-condition ()
                        (warn
                         (make-condition 'simple-style-warning
                                         :format-control
                                         "~S designates an unknown ~
                                               method selector."
                                         :format-arguments
                                         (list ',method-name)))
                        ',method-name))))


;; Optimise constant method names away by converting them to selectors
;; at load-time.
;;
;; Optimise all (SELECTOR ...) forms.  This is important in order to
;; make (FUNCALL (SELECTOR ...) ...) efficient.
(define-compiler-macro selector (&whole form method-name)
  (if (constantp method-name)
      (selector-load-time-form (eval method-name))
      form))


;; Have PRIMITIVE-INVOKE take advantage of the compiler macro for
;; SELECTOR.
(define-compiler-macro primitive-invoke (&whole form
                                         receiver method-name return-type
                                         &rest args)
  (if (constantp method-name)
      `(primitive-invoke ,receiver (selector ,method-name) ,return-type ,@args)
      form))


;; Do the same optimisations for INVOKE-BY-NAME as for PRIMITIVE-INVOKE.
(define-compiler-macro invoke-by-name (&whole form
                                       receiver method-name &rest args)
  (if (constantp method-name)
      `(invoke-by-name ,receiver (selector ,method-name) ,@args)
      form))


;; This compiler macro is a bit more complicated than the preceding
;; ones.
(define-compiler-macro invoke (&whole form
                               receiver message-start &rest message-components)
  (multiple-value-bind (method-name args)
      (split-method-call message-start message-components)
    (if (and (listp method-name)
             (every #'constantp method-name))
        `(invoke-by-name ,receiver (selector ',(mapcar #'eval method-name))
                         ,@args)
        form)))
