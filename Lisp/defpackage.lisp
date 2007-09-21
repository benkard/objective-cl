(defpackage #:mulk.objective-cl
  (:nicknames #:objcl #:objective-cl #:mulk.objcl)
  (:use #:cl #:cffi #:split-sequence)
  (:shadow #:foreign-pointer)

           ;; Functions
  (:export #:initialise-runtime
           #:shutdown-runtime
           #:install-reader-syntax
           #:invoke-by-name
           #:invoke
           #:find-objc-class
           #:find-selector
           #:selector

           ;; Generic functions
           #:objc-eql
           #:objc-equal

           ;; Macros
           #:define-objc-struct
           #:define-objc-union

           ;; Special variables
           #:*trace-method-calls*

           ;; Constants
           #:+nil+
           #:+yes+
           #:+no+

           ;; Classes
           #:id
           #:selector
           #:exception

           ;; Conditions
           #:message-not-understood
           #:no-such-selector

           ;; Metaclasses
           #:objective-c-class))


(defpackage #:mulk.objective-cl-features
  (:nicknames #:objcl-features #:objective-cl-features #:mulk.objcl-features)
  (:use)
  (:export #:gnu-runtime
           #:next-runtime))


#-(or cmu sbcl) (declaim (declaration values))
