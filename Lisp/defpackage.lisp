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

(defpackage #:mulk.objective-cl
  (:nicknames #:objcl #:objective-cl #:mulk.objcl)
  (:use #:closer-common-lisp #:cffi #:split-sequence)
  (:shadow #:foreign-pointer)

           ;; Functions
  (:export #:initialise-runtime
           #:shutdown-runtime
           #:install-reader-syntax
           #:enable-method-syntax
           #:enable-objective-c-syntax
           #:disable-method-syntax
           #:disable-objective-c-syntax
           #:invoke-by-name
           #:invoke
           #:find-objc-class
           #:find-selector
           #:intern-selector
           #:objc-class-name
           #:selector-name
           #:selector
           #:define-returns-boolean-exception
           #:undefine-returns-boolean-exception
           #:collect-classes
           #:collect-methods

           ;; Generic functions
           #:objc-eql
           #:objc-equal
           #:foreign-value-lisp-managed-p

           ;; Macros
           #+(or) #:define-objc-struct
           #+(or) #:define-objc-union
           #:define-objective-c-generic-function
           #:define-objective-c-method
           #:defobjcgeneric
           #:defobjcmethod

           ;; Special variables
           #:*trace-method-calls*

           ;; Constants
           #:+nil+
           #:+yes+
           #:+no+

           ;; Classes
           #:id
           #:selector
           #:exception
           #:foreign-value
           #:foreign-struct
           #:foreign-union
           #:objective-c-generic-function
           #:objective-c-method

           ;; Conditions
           #:message-not-understood
           #:no-such-selector

           ;; Metaclasses
           #:objective-c-class
           #:objective-c-metaclass))


(defpackage #:mulk.objective-cl-features
  (:nicknames #:objcl-features #:objective-cl-features #:mulk.objcl-features)
  (:use)
  (:export #:gnu-runtime
           #:next-runtime))


(defpackage #:ns
  (:nicknames #:objcl-ns #:objective-c-classes #:objc-classes)
  (:use))


(defpackage #:objective-c-methods
  (:nicknames #:objc-methods #:objcm)
  (:use))


(defpackage #:objective-c-selectors
  (:nicknames #:objc-selectors #:objc-sels #:objcs)
  (:use))

#-(or cmu sbcl) (declaim (declaration values))
