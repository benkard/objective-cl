(in-package #:mulk.objective-cl)

(initialise-runtime)

(eval-when (:load-toplevel)
  (unless (boundp '+nil+)
    (defconstant +nil+
      (make-instance 'id :pointer (objcl-get-nil))))
  (unless (boundp '+yes+)
    (defconstant +yes+ (objcl-get-yes)))
  (unless (boundp '+no+)
    (defconstant +no+ (objcl-get-no)))
  (unless (boundp '+runtime-type+)
    (defconstant +runtime-type+ (runtime-type)))
  (pushnew (case +runtime-type+
             ((:gnu) 'objcl-features:gnu-runtime)
             ((:next) 'objcl-features:next-runtime))
           *features*))
