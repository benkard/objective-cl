(defpackage #:mulk.objective-cl
  (:nicknames #:objcl #:objective-cl #:mulk.objcl)
  (:use #:cl #:cffi #:split-sequence)
  (:shadow #:equal
           #:equalp)

           ;; Functions
  (:export #:initialise-runtime
           #:shutdown-runtime
           #:install-reader-syntax
           #:invoke-by-name
           #:invoke
           #:find-objc-class
           #:find-selector

           ;; Generic functions
           #:equal
           #:equalp

           ;; Special variables
           #:*trace-method-calls*

           ;; Classes
           #:id
           #:selector
           #:exception

           ;; Metaclasses
           #:objective-c-class))
