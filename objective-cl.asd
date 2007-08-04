(defsystem "objective-cl"
  :description "A portable Objective C bridge."
  :version "0.0.1"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU General Public License, version 3 or higher"
  :depends-on (#:cffi #:trivial-garbage)
  :components ((:file "defpackage")
               (:file "constant-data")
               (:file "data-types")
               (:file "libobjcl")
               (:file "utilities")
               (:file "weak-hash-tables")
               (:file "memory-management")
               (:file "method-invocation")
               (:file "reader-syntax")
               (:file "objcl"))
  :serial t)
