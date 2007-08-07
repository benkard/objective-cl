(defsystem "objective-cl-tests"
  :description "Unit tests for Objective CL."
  :version "0.0.1"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU General Public License, version 3 or higher"
  :depends-on (#:objective-cl #:lift)
  :components
  ((:module "Lisp"
    :components ((:file "tests")))))
