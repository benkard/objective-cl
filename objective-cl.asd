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
                 (:file "type-conversion"    :depends-on ("defpackage"
                                                          "data-types"))
                 (:file "libobjcl"           :depends-on ("defpackage"
                                                          "data-types"
                                                          "name-conversion"
                                                          "type-conversion"))
                 (:file "utilities"          :depends-on ("defpackage"))
                 (:file "internal-utilities" :depends-on ("defpackage"))
                 (:file "weak-hash-tables"   :depends-on ("defpackage"))
                 (:file "memory-management"  :depends-on ("defpackage"
                                                          "weak-hash-tables"
                                                          "data-types"
                                                          "method-invocation"
                                                          "parameters"))
                 (:file "method-invocation"  :depends-on ("defpackage"
                                                          "name-conversion"
                                                          "type-conversion"
                                                          "libobjcl"
                                                          "internal-utilities"
                                                          "parameters"))
                 (:file "reader-syntax"      :depends-on ("defpackage"
                                                          "method-invocation")))))
  :serial t)
