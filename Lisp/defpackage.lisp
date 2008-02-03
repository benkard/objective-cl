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
  (:use #:cl #:cffi #:split-sequence)
  (:shadow #:foreign-pointer)

           ;; Functions
  (:export #:initialise-runtime
           #:shutdown-runtime
           #:install-reader-syntax
           #:invoke-by-name
           #:invoke
           #:find-objc-class
           #:find-selector
           #:objc-class-name
           #:selector-name
           #:selector

           ;; Generic functions
           #:objc-eql
           #:objc-equal

           ;; Macros
           #+(or) #:define-objc-struct
           #+(or) #:define-objc-union

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

           ;; Conditions
           #:message-not-understood
           #:no-such-selector

           ;; Metaclasses
           #:objective-c-class))


(defpackage #:mulk.objective-cl-features
  (:nicknames #:objcl-features #:objective-cl-features #:mulk.objcl-features)
  (:use)
  (:export #:gnu-runtime
           #:next-runtime))


(defpackage #:ns
  (:nicknames #:objcl-ns #:objective-c-classes #:objc-classes)
  (:use))


#-(or cmu sbcl) (declaim (declaration values))
