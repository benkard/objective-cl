(in-package #:mulk.objective-cl)


(define-condition simple-style-warning (style-warning)
  ((format-control :initarg :format-control
                   :reader format-control)
   (format-arguments :initarg :format-arguments
                     :reader format-arguments))
  (:report (lambda (condition stream)
             (apply #'format
                    stream
                    (format-control condition)
                    (format-arguments condition)))))


(define-condition no-such-selector (error)
  ((designator :initarg :designator
               :reader rejected-selector-designator))
  (:report (lambda (condition stream)
             ;; The CLHS forbids the use of WITH-SLOTS for conditions.
             (format stream
                     "~S does not designate a known selector."
                     (rejected-selector-designator condition)))))


(define-condition message-not-understood (error)
  ((selector :initarg :selector
             :reader rejected-selector)
   (class :initarg :class
          :reader rejecting-class))
  (:report (lambda (condition stream)
             (format stream
                     "The Objective-C class ~S does not understand the ~
                         message ~S."
                     (rejecting-class condition)
                     (rejected-selector condition)))))
