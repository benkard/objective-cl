(in-package #:mulk.objective-cl)


(define-condition no-such-selector (error)
  ((designator :initarg :designator
               :reader rejected-selector-designator))
  (:report (lambda (condition stream)
             (with-slots (designator) condition
                (format stream
                        "~S does not designate a known selector."
                        designator)))))


(define-condition message-not-understood (error)
  ((selector :initarg :selector
             :reader rejected-selector)
   (class :initarg :class
          :reader rejecting-class))
  (:report (lambda (condition stream)
             (with-slots (selector class) condition
                (format stream
                        "The Objective-C class ~S does not understand the ~
                         message ~S."
                        class
                        selector)))))
