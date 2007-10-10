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


;; Optimise constant method names away by converting them to selectors
;; at load-time.
(define-compiler-macro primitive-invoke (&whole form
                                         receiver method-name return-type
                                         &rest args)
  (if (and (constantp method-name)
           (not (and (listp method-name)
                     (eq 'load-time-value (car method-name)))))
      `(primitive-invoke ,receiver
                         (load-time-value (handler-case
                                              (find-selector ,method-name t)
                                            (serious-condition ()
                                              (warn
                                               (make-condition
                                                'simple-style-warning
                                                :format-control
                                                "~S designates an unknown ~
                                                 method selector."
                                                :format-arguments
                                                (list ,method-name)))
                                              ,method-name)))
                         ,return-type ,@args)
      form))


;; Do the same optimisations for INVOKE-BY-NAME as for PRIMITIVE-INVOKE.
(define-compiler-macro invoke-by-name (&whole form
                                       receiver method-name &rest args)
  (if (and (constantp method-name)
           (not (and (listp method-name)
                     (eq 'load-time-value (car method-name)))))
      `(invoke-by-name
        ,receiver
        (load-time-value (handler-case
                             (find-selector ,method-name t)
                           (serious-condition ()
                             (warn
                              (make-condition 'simple-style-warning
                                              :format-control
                                              "~S designates an unknown ~
                                               method selector."
                                              :format-arguments
                                              (list ,method-name)))
                             ,method-name)))
        ,@args)
      form))


;; This compiler macro is a bit more complicated than the preceding
;; ones.
(define-compiler-macro invoke (receiver message-start &rest message-components)
  (multiple-value-bind (method-name args)
      (split-method-call (if (and (consp message-start)
                                  (eq (first message-start) 'quote))
                             (second message-start)
                             message-start)
                         message-components)
    `(invoke-by-name
      ,receiver
      (load-time-value (handler-case
                           (find-selector ',method-name t)
                         (serious-condition ()
                           (warn
                            (make-condition 'simple-style-warning
                                            :format-control
                                            "~S designates an unknown ~
                                               method selector."
                                            :format-arguments
                                            (list ',method-name)))
                           ',method-name)))
      ,@args)))
