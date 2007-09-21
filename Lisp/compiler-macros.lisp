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
                                              (selector ,method-name)
                                            (serious-condition ()
                                              (warn
                                               (make-condition
                                                'style-warning
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
                             (selector ,method-name)
                           (serious-condition ()
                             (warn
                              (make-condition 'style-warning
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
      (split-method-call message-start message-components)
    `(invoke-by-name
      ,receiver
      (load-time-value (handler-case
                           (selector ',method-name)
                         (serious-condition ()
                           (warn
                            (make-condition 'style-warning
                                            :format-control
                                            "~S designates an unknown ~
                                               method selector."
                                            :format-arguments
                                            (list ',method-name)))
                           ',method-name)))
      ,@args)))
