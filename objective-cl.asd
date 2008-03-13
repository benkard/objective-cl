;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007, 2008  Matthias Andreas Benkard.
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

(defvar asdf::*objcl-version* "0.2.1")


#+@use-prebuilt-objective-cl@
(defpackage #:objcl-asdf
  (:use #:cl #:asdf)
  (:export #:objc-source-file
           #:*objc-obj-dir*))

#+@use-prebuilt-objective-cl@
(in-package #:objcl-asdf)

#+@use-prebuilt-objective-cl@
(defvar *objc-obj-dir*
    @prebuilt-objective-cl-location@)

#+@use-prebuilt-objective-cl@
(in-package #:asdf)


(defsystem "objective-cl"
  :description "A portable Objective C bridge."
  :version asdf::*objcl-version*
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU Lesser General Public License, version 3 or higher"
  :depends-on (#:cffi #:trivial-garbage #:split-sequence #:closer-mop
               #-@use-prebuilt-objective-cl@ #:objective-cl-libobjcl)
  :components
  ((:module "Lisp"
    :components ((:file "defpackage")
                 (:file "internal-reader-syntax" :depends-on ("defpackage"))
                 (:file "constant-data"      :depends-on ("defpackage"
                                                          "internal-reader-syntax"))
                 (:file "conditions"         :depends-on ("defpackage"))
                 (:file "parameters"         :depends-on ("defpackage"))
                 (:file "data-types"         :depends-on ("defpackage"
                                                          "conditions"))
                 (:file "internal-utilities" :depends-on ("defpackage"
                                                          "data-types"))
                 (:file "name-conversion"    :depends-on ("defpackage"
                                                          "internal-utilities"))
                 (:file "weak-hash-tables"   :depends-on ("defpackage"))
                 (:file "performance-hacks"  :depends-on ("defpackage"))
                 (:file "policy"             :depends-on ("defpackage"
                                                          "parameters"
                                                          "libobjcl"))
                 (:file "libobjcl"           :depends-on ("defpackage"
                                                          "internal-reader-syntax"
                                                          "constant-data"
                                                          "data-types"
                                                          "name-conversion"
                                                          "internal-utilities"
                                                          "parameters"
                                                          "conditions"
                                                          "memory-management"))
                 (:file "init"               :depends-on ("defpackage"
                                                          "libobjcl"))
                 (:file "type-conversion"    :depends-on ("defpackage"
                                                          "memory-management"
                                                          "data-types"
                                                          "type-handling"
                                                          "policy"
                                                          "data-types"))
                 (:file "type-conversion-policy" :depends-on ("defpackage"
                                                              "memory-management"
                                                              "data-types"
                                                              "type-handling"
                                                              "policy"
                                                              "data-types"
                                                              "method-invocation"
                                                              "lisp-value-wrapping"))
                 (:file "type-handling"      :depends-on ("defpackage"
                                                          "libobjcl"
                                                          "init"))
                 (:file "memory-management"  :depends-on ("defpackage"
                                                          "weak-hash-tables"
                                                          "parameters"
                                                          "conditions"))
                 (:file "method-invocation"  :depends-on ("defpackage"
                                                          "type-conversion"
                                                          "type-handling"
                                                          "name-conversion"
                                                          "data-types"
                                                          "libobjcl"
                                                          "internal-utilities"
                                                          "parameters"
                                                          "init"
                                                          "conditions"
                                                          "memory-management"))
                 (:file "reader-syntax"      :depends-on ("defpackage"
                                                          "method-invocation"
                                                          "internal-reader-syntax"))
                 (:file "utilities"          :depends-on ("init"
                                                          "defpackage"
                                                          "method-invocation"
                                                          "data-types"))
                 (:file "class-definition"   :depends-on ("defpackage"
                                                          "libobjcl"
                                                          "type-conversion"
                                                          "type-handling"
                                                          "data-types"
                                                          "method-definition"))
                 (:file "instance-management" :depends-on ("defpackage"
                                                           "libobjcl"
                                                           "init"
                                                           "method-invocation"))
                 (:file "compiler-macros"    :depends-on ("defpackage"
                                                          "method-invocation"
                                                          "conditions"))
                 (:file "method-definition" :depends-on ("defpackage"
                                                         "libobjcl"
                                                         "init"
                                                         "method-invocation"
                                                         "data-types"))
                 (:file "lisp-value-wrapping" :depends-on ("defpackage"
                                                           "libobjcl"
                                                           "init"
                                                           "class-definition"
                                                           "method-invocation"
                                                           "method-definition"
                                                           "reader-syntax"
                                                           "data-types"))
                 (:file "post-init"          :depends-on ("defpackage"
                                                          "libobjcl"
                                                          "init"
                                                          "class-definition")))))
  :serial t)
