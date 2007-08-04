(defpackage #:mulk.objective-cl
  (:nicknames #:objcl)
  (:use #:cl #:cffi #:split-sequence)
  (:export #:initialise-runtime
           #:shutdown-runtime
           #:install-reader-syntax
           #:invoke-by-name
           #:invoke
           #:find-objc-class
           #:*trace-method-calls*))
