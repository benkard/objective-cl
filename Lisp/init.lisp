(in-package #:mulk.objective-cl)

(initialise-runtime)

(eval-when (:load-toplevel)
  (unless (boundp '+nil+)
    (defconstant +nil+
      (make-instance 'id :pointer (objcl-get-nil))))
  (unless (boundp '+yes+)
    (defconstant +yes+ (objcl-get-yes)))
  (unless (boundp '+no+)
    (defconstant +no+ (objcl-get-no))))
