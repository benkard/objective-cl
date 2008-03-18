;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007  Matthias Andreas Benkard.
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


(defvar *global-lock* (cons nil nil))


(defmacro atomically (&body body)
  `(with-exclusive-access (*global-lock*)
     ,@body))


(defmacro with-exclusive-access ((&rest objects) &body body)
  (etypecase objects
    (null `(progn ,@body))
    (cons `(with-lock ,(first objects)
             (with-exclusive-access ,(rest objects)
               ,@body)))))


(defmacro with-lock (object &body body)
  ;; FIXME: Implement LOCK-FOR-OBJECT.
  `(progn ,@body)
  #+(or)
  (let ((lock (gensym "LOCK")))
    `(let ((,lock (lock-for-object ,object)))
       (prog2
         (%objcl-acquire-lock ,lock)
         (progn ,@body)
         (%objcl-release-lock ,lock)))))


(defun featurep (symbol)
  (member symbol *features*))


(defmacro with-foreign-string-pool ((register-fn-name
                                     allocate-fn-name) &body body)
  (let ((pool-var (gensym)))
    `(let ((,pool-var (list)))
       (labels ((,register-fn-name (x)
                  (push x ,pool-var)
                  x)
                (,allocate-fn-name (string)
                  (,register-fn-name
                   (cffi:foreign-string-alloc string))))
         (unwind-protect
             (progn ,@body)
           (dolist (x ,pool-var)
             (cffi:foreign-string-free x)))))))


(defmacro with-foreign-object-pool ((register-fn-name) &body body)
  (let ((pool-var (gensym)))
    `(let ((,pool-var (list)))
       (flet ((,register-fn-name (x)
                (push x ,pool-var)
                x))
         (unwind-protect
             (progn ,@body)
           (dolist (x ,pool-var)
             (cffi:foreign-free x)))))))


(defmacro defcoercion (to-class (object) &body body)
  (let ((type-sym (gensym)))
    `(defmethod coerce-object (,object (,type-sym (eql ',to-class)))
       ,@body)))


;; Compatibility with older versions of CFFI.
(unless (fboundp 'foreign-funcall-pointer)
  (defmacro foreign-funcall-pointer (pointer options &rest args)
    (let ((foreign-funcall-pointer-sym (find-symbol "FOREIGN-FUNCALL-POINTER" '#:cffi)))
      (if foreign-funcall-pointer-sym
          `(,foreign-funcall-pointer-sym ,pointer ,options ,@args)
          `(cffi:foreign-funcall (,pointer ,@options) ,@args)))))


;; Caching of function values.
(defmacro define-cached-function (name lambda-list hashing-form &body body)
  "Define a function and cache its result in a hash table.  The hash key
is computed according to HASHING-FORM.

Note that HASHING-FORM is expected to return a freshly consed object
that can be garbage collected.  It is not recommended to use an atom as
a HASHING-FORM if the value of the atom might itself be an interned atom
or any other value that might never be deleted by the garbage
collector."
  (let ((hash-table (gensym))
        (value (gensym))
        (default-value (gensym))
        (hash-key (gensym))
        (no-weak-hashing-p (handler-case
                               (prog1 nil
                                 (tg:make-weak-hash-table :weakness :key
                                                          :test 'equal))
                             (serious-condition () t))))
    (if no-weak-hashing-p
        `(defun ,name ,lambda-list ,@body)
        `(let ((,hash-table (tg:make-weak-hash-table :weakness :key
                                                     :test 'equal)))
           (defun ,name ,lambda-list
             (let* ((,hash-key ,hashing-form)
                    (,value (gethash ,hash-key ,hash-table ',default-value)))
               (if (eq ',default-value ,value)
                   (values-list
                    (setf (gethash ,hash-key ,hash-table)
                          (multiple-value-list (progn ,@body))))
                   (values-list ,value))))))))
