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

(defsystem "objective-cl"
  :description "A portable Objective C bridge."
  :version "0.0.4"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU General Public License, version 3 or higher"
  :depends-on (#:cffi #:trivial-garbage #:split-sequence #:objective-cl-libobjcl
               #:closer-mop)
  :components
  ((:module "Lisp"
    :components ((:file "defpackage")
                 (:file "constant-data"      :depends-on ("defpackage"))
                 (:file "data-types"         :depends-on ("defpackage"
                                                          "conditions"))
                 (:file "parameters"         :depends-on ("defpackage"))
                 (:file "name-conversion"    :depends-on ("defpackage"))
                 (:file "internal-utilities" :depends-on ("defpackage"))
                 (:file "weak-hash-tables"   :depends-on ("defpackage"))
                 (:file "conditions"         :depends-on ("defpackage"))
                 (:file "performance-hacks"  :depends-on ("defpackage"))
                 (:file "libobjcl"           :depends-on ("defpackage"
                                                          "constant-data"
                                                          "data-types"
                                                          "name-conversion"
                                                          "internal-utilities"
                                                          "parameters"
                                                          "conditions"
                                                          "memory-management"))
                 (:file "init"               :depends-on ("defpackage"
                                                          "libobjcl"))
                 (:file "type-handling"      :depends-on ("defpackage"
                                                          "libobjcl"
                                                          "init"))
                 (:file "method-invocation"  :depends-on ("defpackage"
                                                          "type-handling"
                                                          "name-conversion"
                                                          "data-types"
                                                          "libobjcl"
                                                          "internal-utilities"
                                                          "parameters"
                                                          "init"
                                                          "conditions"
                                                          "memory-management"))
                 (:file "memory-management"  :depends-on ("defpackage"
                                                          "weak-hash-tables"
                                                          "parameters"))
                 (:file "reader-syntax"      :depends-on ("defpackage"
                                                          "method-invocation"))
                 (:file "utilities"          :depends-on ("init"
                                                          "defpackage"
                                                          "method-invocation"
                                                          "data-types"))
                 (:file "class-definition"   :depends-on ("defpackage"
                                                          "libobjcl"
                                                          "init"
                                                          "method-invocation"
                                                          "data-types"))
                 (:file "compiler-macros"    :depends-on ("defpackage"
                                                          "method-invocation"
                                                          "conditions")))))
  :serial t)


(defpackage objcl-asdf
  (:use #:cl #:asdf)
  (:export #:objc-source-file
           #:*objc-obj-dir*))
(in-package #:objcl-asdf)

(defvar *objc-obj-dir*)

(defclass objc-source-file (source-file) ())
(defclass objcl-c-source-file (objc-source-file) ()
  (:documentation "An Objective-CL C source file"))

(defmethod source-file-type ((c objc-source-file) (s module)) "m")
(defmethod source-file-type ((c objcl-c-source-file) (s module)) "c")

(defmethod perform :before (o (c objc-source-file))
  ;; Copy the Objective-C sources to the target directory.
  (unless (null (output-files o c))
    (let* ((source-dir (component-pathname (find-system "objective-cl-libobjcl")))
           (sources
            (mapcar #'(lambda (x)
                        (enough-namestring x source-dir))
                    (mapcan #'(lambda (x)
                                (directory (merge-pathnames x source-dir)))
                            '(#p"**/*.c" #p"**/*.m" #p"**/*.h"
                              #p"**/GNUmakefile.*"
                              #p"**/*.make" #p"**/GNUmakefile"
                              #p"**/*.in" #p"**/configure" #p"**/configure.ac"
                              #p"libffi/**/*" #p"libffi/**/*.*"))))
           (output-dir
            (merge-pathnames #p"../../"
                             (directory-namestring (first (output-files o c))))))
      (dolist (relative-source-file sources)
        (let ((output-file (merge-pathnames relative-source-file output-dir))
              (source-file (merge-pathnames relative-source-file source-dir)))
          (ensure-directories-exist output-file)
          (unless (and (probe-file output-file)
                       (= (file-write-date source-file)
                          (file-write-date output-file)))
            (ignore-errors  ;; FIXME: We need to skip directories, so
                            ;; that IGNORE-ERRORS can go away.
              (with-open-file (in source-file
                                  :element-type '(unsigned-byte 8))
                (with-open-file (out output-file
                                     :direction :output
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
                  (loop for byte = (read-byte in nil nil)
                        while byte
                        do (write-byte byte out)))))))))))

(defmethod perform ((o compile-op) (c objc-source-file))
  (unless (or (operation-done-p o c)
              (null (output-files o c)))
    (zerop
     ;; Run `make' at the top level of the directory tree.
     (run-shell-command "make -C '~A'"
                        (merge-pathnames #p"../../"
                                         (directory-namestring
                                          (first (output-files o c))))))))

(defmethod output-files :around ((o compile-op) (c objc-source-file))
  ;; If this doesn't get called at all, we're kind of screwed.
  (let ((files (call-next-method)))
    (setq *objc-obj-dir*
          (merge-pathnames #p"../"
                           (directory-namestring (first files))))
    files))

#+(or)
(defmethod output-files ((o compile-op) (c objc-source-file))
  (print (list (merge-pathnames (make-pathname :directory '(:relative "Objective-C" "obj")
                                        :type "o")
                         (merge-pathnames
                          (component-pathname (find-system "objective-cl-libobjcl"))
                          (component-pathname c))))))

(defmethod output-files ((o compile-op) (c objc-source-file))
  (list (merge-pathnames (make-pathname :directory '(:relative "obj")
                                        :type "o")
                         (component-pathname c))))

(defmethod operation-done-p ((o compile-op) (c objc-source-file))
  (and (every #'probe-file (output-files o c))
       (> (loop for file in (output-files o c)
                minimizing (file-write-date file))
          (file-write-date (component-pathname c)))))

(defmethod perform ((o load-op) (c objc-source-file))
  nil)


(defsystem "objective-cl-libobjcl"
  :description "A portable Objective C bridge."
  :version "0.0.4"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU General Public License, version 3 or higher"
  :depends-on ()
  :components ((:module "Objective-C"
                        :components ((:objc-source-file "libobjcl")
                                     (:objc-source-file "PyObjC/libffi_support")
                                     (:objc-source-file "PyObjC/objc_support")
                                     (:objc-source-file "PyObjC/objc-runtime-apple")
                                     (:objc-source-file "PyObjC/objc-runtime-compat")
                                     (:objc-source-file "PyObjC/objc-runtime-gnu")
                                     (:objc-source-file "JIGS/ObjcRuntimeUtilities2")
                                     (:objcl-c-source-file "JIGS/ObjcRuntimeUtilities"))))
  :serial t)
