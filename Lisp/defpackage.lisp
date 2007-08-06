(defpackage #:mulk.objective-cl
  (:nicknames #:objcl #:objective-cl #:mulk.objcl)
  (:use #:cl #:cffi #:split-sequence)

           ;; Functions
  (:export #:initialise-runtime
           #:shutdown-runtime
           #:install-reader-syntax
           #:invoke-by-name
           #:invoke
           #:find-objc-class
           #:find-selector

           ;; Special variables
           #:*trace-method-calls*

           ;; Classes
           #:id
           #:selector
           #:exception

           ;; Metaclasses
           #:objective-c-class))
