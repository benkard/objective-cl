(defsystem "objective-cl"
  :description "A portable Objective C bridge."
  :version "0.0.1"
  :author "Matthias Benkard <matthias@benkard.de>"
  :licence "GNU General Public License, version 3 or higher"
  :depends-on (#:cffi #:trivial-garbage #:split-sequence)
  :components
  ((:module "Lisp"
    :components ((:file "defpackage")
                 (:file "constant-data"      :depends-on ("defpackage"))
                 (:file "data-types"         :depends-on ("defpackage"))
                 (:file "parameters"         :depends-on ("defpackage"))
                 (:file "name-conversion"    :depends-on ("defpackage"))
                 (:file "internal-utilities" :depends-on ("defpackage"))
                 (:file "weak-hash-tables"   :depends-on ("defpackage"))
                 (:file "libobjcl"           :depends-on ("defpackage"
                                                          "constant-data"
                                                          "data-types"
                                                          "name-conversion"
                                                          "internal-utilities"
                                                          "parameters"))
                 (:file "init"               :depends-on ("libobjcl"))
                 (:file "method-invocation"  :depends-on ("defpackage"
                                                          "name-conversion"
                                                          "data-types"
                                                          "libobjcl"
                                                          "internal-utilities"
                                                          "parameters"))
                 (:file "memory-management"  :depends-on ("defpackage"
                                                          "init"
                                                          "weak-hash-tables"
                                                          "data-types"
                                                          "method-invocation"
                                                          "parameters"))
                 (:file "reader-syntax"      :depends-on ("defpackage"
                                                          "method-invocation"))
                 (:file "utilities"          :depends-on ("init"
                                                          "defpackage"
                                                          "method-invocation"
                                                          "data-types")))))
  :serial t)
