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
                     (rejected-selector-designator condition))))
  (:documentation "Someone tried to dereference an invalid method selector.

## Description:

A **condition** of **type** __no-such-selector__ is **signal**led when
an attempt is made to retrieve a method selector by name, and that
selector cannot be found.  This is most often the case when a typo is
made in the message name part of a method invocation."))


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
                     (rejected-selector condition))))
  (:documentation "The runtime is unable to retrieve the signature of a method.

## Description:

A **condition** of **type** __message-not-understood__ is **signal**led
when an attempt is made to call a method on an object that knows nothing
of it and can therefore not provide signature information for the
method.  This is most often the case when an object is used at a place
that expects an object of a different Objective-C type (that is, it can
be regarded as a kind of type error), although it can also happen when a
typo is made in the message name part of a method invocation and a
__no-such-selector__ error is not **signal**led."))
