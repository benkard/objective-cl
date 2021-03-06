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

(defpackage objcl-asdf
  (:use #:cl #:asdf)
  (:export #:objc-source-file
           #:*objc-obj-dir*))
(in-package #:objcl-asdf)

(defvar *objc-obj-dir*)
(defvar *stuff-copied-p* nil)

(defclass objc-source-file (source-file) ())
(defclass objcl-c-source-file (objc-source-file) ()
  (:documentation "An Objective-CL C source file"))

(defmethod source-file-type ((c objc-source-file) (s module)) "m")
(defmethod source-file-type ((c objcl-c-source-file) (s module)) "c")

(defmethod perform ((o compile-op) (c objc-source-file))
  (unless (or (operation-done-p o c)
              (null (output-files o c)))
    (zerop
     ;; Run `make' at the top level of the directory tree.
     (let (#+allegro (asdf::*verbose-out* (if (find-package '#:swank)
                                              nil
                                              asdf::*verbose-out*)))
       (run-shell-command "make -C '~A'"
                          (namestring
                           (make-pathname
                            :directory
                            (pathname-directory
                             (merge-pathnames
                              (make-pathname :directory '(:relative ".." ".."))
                              (first
                               (output-files
                                o
                                (find "libobjcl"
                                      (module-components
                                       (first
                                        (module-components
                                         (find-system "objective-cl-libobjcl"))))
                                      :key #'component-name
                                      :test #'string=))))))))))))

(defmethod output-files :around ((o compile-op) (c objc-source-file))
  ;; If this doesn't get called at all, we're kind of screwed.
  (let ((files (call-next-method)))
    (when (string= (component-name c) "libobjcl")
    (setq *objc-obj-dir*
          (merge-pathnames #p"../"
                           (directory-namestring (first files)))))
    files))

(defmethod output-files ((o compile-op) (c objc-source-file))
  (let ((relative-source-file
         (enough-namestring (component-pathname c)
                            (merge-pathnames
                             (make-pathname :directory '(:relative "Objective-C"))
                             (component-pathname
                              (find-system "objective-cl-libobjcl"))))))
    (list (merge-pathnames
           (make-pathname :type "o")
           (merge-pathnames
            relative-source-file
            (merge-pathnames
             (make-pathname :directory '(:relative "Objective-C" "obj"))
             (component-pathname (find-system "objective-cl-libobjcl"))))))))

(defmethod operation-done-p ((o compile-op) (c objc-source-file))
  (and (every #'probe-file (output-files o c))
       (> (loop for file in (output-files o c)
                minimizing (file-write-date file))
          (file-write-date (component-pathname c)))))

(defmethod perform ((o load-op) (c objc-source-file))
  nil)


(defsystem "objective-cl-libobjcl"
  :description "A portable Objective C bridge."
  :version asdf::*objcl-version*
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU Lesser General Public License, version 3 or higher"
  :depends-on ()
  :components ((:module "Objective-C"
                        :components
                        ((:objc-source-file "libobjcl")
                         (:objc-source-file "NSObject-ObjectiveCLWrapperLink")
                         (:module "PyObjC"
                                  :components
                                  ((:objc-source-file "libffi_support")
                                   (:objc-source-file "objc_support")
                                   (:objc-source-file "objc-runtime-apple")
                                   (:objc-source-file "objc-runtime-compat")
                                   (:objc-source-file "objc-runtime-gnu")))
                         (:module "JIGS"
                                  :components
                                  ((:objc-source-file "ObjcRuntimeUtilities2")
                                   (:objcl-c-source-file "ObjcRuntimeUtilities"))))))
  :serial t)


(defun sanitise-dir-name (pathname)
  ;; "/bla/stuff///" -> "/bla/stuff"
  (loop with dir-name = (namestring pathname)
        while (char= #\/ (elt dir-name (1- (length dir-name))))
        do (setq dir-name (subseq dir-name 0 (1- (length dir-name))))
        finally (return-from sanitise-dir-name dir-name)))


(defmethod perform :before (o (c objc-source-file))
  ;; Copy the Objective-C sources to the target directory.
  (let ((output-files
         (output-files (make-instance 'compile-op)
                       (find "libobjcl"
                             (module-components
                              (first
                               (module-components
                                (find-system "objective-cl-libobjcl"))))
                             :key #'component-name
                             :test #'string=))))
    (unless (or *stuff-copied-p* (null output-files))
      (setq *stuff-copied-p* t)
      (let* ((source-dir (component-pathname (find-system "objective-cl-libobjcl")))
             (output-dir
              (merge-pathnames #p"../../"
                               (directory-namestring (first output-files))))
             (output-parent-dir (merge-pathnames #p"../" output-dir))
             #+allegro (asdf::*verbose-out* (if (find-package '#:swank)
                                                nil
                                                asdf::*verbose-out*)))
        ;; First try using cp to copy the files over to the compilation
        ;; directory.  If that fails, do it manually, file by file.
        (unless (and (not (position #\' (namestring source-dir))) ; a safety measure
                     (not (position #\' (namestring output-parent-dir)))
                     ;; If the directories' inode numbers are the same,
                     ;; we obviously don't need to copy anything.
                     ;;
                     ;; But first, do some sanity checks about the
                     ;; environment.
                     (ignore-errors
                       (ensure-directories-exist output-parent-dir)
                       (or (and (zerop (run-shell-command "ls -d -i '~A'"
                                                          source-dir))
                                (zerop (run-shell-command "ls -d -i '~A'"
                                                          output-parent-dir))
                                (zerop (run-shell-command "echo"))
                                (zerop (run-shell-command "echo | awk '{ print $1 }'"))
                                (zerop
                                 (run-shell-command
                                  "test ~
                                     \"x$(ls -d -i '~A' | awk '{ print $1 }')\" ~
                                     = ~
                                     \"x$(ls -d -i '~A' | awk '{ print $1 }')\""
                                  source-dir
                                  output-parent-dir)))
                           (zerop (run-shell-command "cp -R -P -f -p '~A' '~A/'"
                                                     (sanitise-dir-name source-dir)
                                                     output-parent-dir))))
                     (progn
                       (ensure-directories-exist output-dir)
                       (probe-file (merge-pathnames "GNUmakefile" output-dir))))
          ;; We couldn't use cp.  Copy the files manually.
          (let ((sources
                 (mapcar #'(lambda (x)
                             (enough-namestring x source-dir))
                         (mapcan #'(lambda (x)
                                     (directory (merge-pathnames x source-dir)))
                                 '(#p"**/*.c" #p"**/*.m" #p"**/*.h"
                                   #p"**/GNUmakefile.*"
                                   #p"**/*.make" #p"**/GNUmakefile"
                                   #p"**/*.in" #p"**/configure" #p"**/configure.ac"
                                   #p"libffi-3.0.4/**/*" #p"libffi-3.0.4/**/*.*")))))
            (dolist (relative-source-file sources)
              (let ((output-file (merge-pathnames relative-source-file output-dir))
                    (source-file (merge-pathnames relative-source-file source-dir)))
                (ensure-directories-exist output-file)
                (unless (and (probe-file output-file)
                             (= (file-write-date source-file)
                                (file-write-date output-file)))
                  (ignore-errors
                    ;; FIXME: We need to skip directories, so that
                    ;; IGNORE-ERRORS can go away.
                    (with-open-file (in source-file
                                        :element-type '(unsigned-byte 8))
                      (with-open-file (out output-file
                                           :direction :output
                                           :if-exists :supersede
                                           :element-type '(unsigned-byte 8))
                        (loop for byte = (read-byte in nil nil)
                              while byte
                              do (write-byte byte out))))))))))))))
