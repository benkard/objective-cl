(defsystem "objective-cl-clozure-compat"
  :description "Different package names for Objective-CL that clash with
 Clozure-CL's predefined ones."
  :version "0.0.4"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU Lesser General Public License, version 3 or higher"
  :depends-on (:objective-cl)
  :components ((:module "Lisp"
                :components ((:file "clozure-compat"))))
  :serial t)
