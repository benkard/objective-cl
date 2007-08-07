(defpackage #:mulk.objective-cl.tests
  (:use #:cl #:lift #:mulk.objective-cl))
(in-package #:mulk.objective-cl.tests)


(defun run-all-tests ()
  (run-tests :suite 'objective-cl))


(deftestsuite objective-cl ()
  ())


(deftestsuite method-invocation (objective-cl)
  ()
  (:equality-test #'equal)
  (:tests
   ((ensure (functionp (fn #'+ _ 10))))
   ((ensure-same (mapcar (fn (cons _ _)) '(1 2 3))
                 '((1 . 1) (2 . 2) (3 . 3))))))
