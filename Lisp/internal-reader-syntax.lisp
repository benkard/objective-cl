;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007, 2008  Matthias Andreas Benkard.
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public License
;;;; as published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(in-package #:mulk.objective-cl)


(defvar *readtable-stack* (list))


(defun restore-readtable ()
  (when *readtable-stack*
    (setq *readtable* (pop *readtable-stack*)))
  (values))


(defun save-readtable ()
  (push *readtable* *readtable-stack*)
  (setq *readtable* (copy-readtable *readtable*))
  (values))


(defun in-type-declaration-syntax ()
  (setq *readtable* (copy-readtable *readtable*))
  (set-dispatch-macro-character #\# #\? #'read-type-declaration)
  (values))


(defun enable-type-declaration-syntax ()
  (save-readtable)
  (set-dispatch-macro-character #\# #\? #'read-type-declaration)
  (values))


(defun disable-type-declaration-syntax ()
  (restore-readtable)
  (values))


(defun read-type-declaration (stream subchar argument)
  (declare (ignore argument))
  (let (defun-form typedecl-string)
    (loop with finished-p = nil
          with next-char = nil
          with next-subchar = nil
          until finished-p
          for char = (read-char stream nil nil t)
          while char
          if (char= char #\Newline)
            if (and (setq next-char (read-char stream nil nil t))
                    (char= next-char #\#))
              if (and (setq next-subchar (read-char stream nil nil t))
                      (char= next-subchar subchar))
                 do (progn)
              else do (progn
                        (cerror "Ignore the current line."
                                "Unknown macro subcharacter ~S found while ~
                                 reading type declaration."
                                next-subchar)
                        (read-line stream t nil t)
                        (unread-char #\Newline stream))
            else do (progn
                      (unread-char next-char stream)
                      (setq defun-form (read stream t nil t))
                      (setq finished-p t))
          else
            collect char into collected-chars
          finally
            (setq typedecl-string (coerce collected-chars 'string)))
    (let* ((eof-value (gensym))
           (typedecl (with-input-from-string (in typedecl-string)
                       (loop for x = (read in nil eof-value nil)
                             until (eq x eof-value)
                             collect x)))
           (typedecl-parts
            (split-sequence:split-sequence '->
                                           typedecl
                                           :test #'(lambda (x y)
                                                     (and (symbolp x)
                                                          (symbolp y)
                                                          (string=
                                                           (symbol-name x)
                                                           (symbol-name y))))
                                           :count 2))
           (arg-types (first typedecl-parts))
           (return-types (second typedecl-parts))
           (return-type `(values ,@return-types
                                 ,@(unless
                                       (member '&rest return-types)
                                     '(&rest nil)))))
      (destructuring-bind (head function-name lambda-list . body) defun-form
        (let* ((decls-end (position-if #'(lambda (form)
                                           (not (or (stringp form)
                                                    (and (listp form)
                                                         (eq (first form)
                                                             'declare)))))
                                       body))
               (declarations-and-docs (subseq body 0 decls-end))
               (real-body (subseq body decls-end)))
        `(progn
           (declaim (ftype (function (,@arg-types) ,return-type)
                           ,function-name))
           (,head ,function-name ,lambda-list
             ,@declarations-and-docs
             (declare
               ,@(loop for arg in lambda-list
                       for type in arg-types
                       for arg-name = (cond ((atom arg) arg)
                                            (t (car arg)))
                       unless (or (member arg lambda-list-keywords)
                                  (and (symbolp type)
                                       (string= (symbol-name type) "*")))
                         collect `(type ,type ,arg-name)))
             (the ,return-type
               ,@real-body))))))))


;; Quick test.
#||
\(progn
   #.(disable-type-declaration-syntax)
   #.(enable-type-declaration-syntax)
   (pprint
    '#? symbol t * &rest list ->
    (defun typespec-name->type-id (typespec-name x y &rest rest)
      "abc"
      (declare foo)
      "mulk"
      (type-name->type-id (typespec-name->type-name typespec-name)))))
||#
