(in-package #:mulk.objective-cl)


(defun apply-macro (macro-name arg &rest args)
  "Because FOREIGN-FUNCALL is a macro.  Why, oh why is this?"
  (funcall
   (compile nil
            `(lambda ()
               (,macro-name ,@(butlast (cons arg args))
                            ,@(car (last (cons arg args))))))))