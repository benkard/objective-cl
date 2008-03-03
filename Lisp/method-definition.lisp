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

(in-package #:objective-cl)


(defclass objective-c-generic-function (standard-generic-function)
     ()
  (:metaclass funcallable-standard-class))


(defclass objective-c-method (standard-method)
     ((return-type :initarg :return-type
                   :accessor method-return-type)
      (argument-types :initarg :argument-types
                      :accessor method-argument-types))
  (:metaclass standard-class))


(defun qualifiers-return-type (qualifiers)
  (find-if #'(lambda (x)
               (or (not (keywordp x))
                   (typep x 'objective-c-type-keyword)))
           qualifiers))


(defmacro define-objective-c-method (name &rest args)
  (let ((qualifiers (list)))
    (loop until (listp (first args))
          do (push (pop args) qualifiers))
    (setq qualifiers (nreverse qualifiers))
    (destructuring-bind (lambda-list . body) args
      (let ((lambda-list (copy-list lambda-list)))
        (loop for arg-cons on lambda-list
              for arg = (car arg-cons)
              until (member arg lambda-list-keywords)
              if (listp arg)
                if (typep (second arg) 'objective-c-type-keyword)
                  collect (second arg) into type-specifiers
                  and do (setf (car arg-cons) (first arg))
                else
                  ;; We simply map all non-OBJECTIVE-C-TYPE-KEYWORD
                  ;; specialisers to :ID.  This makes sense: If the
                  ;; specialiser is an Objective-C type, the type
                  ;; specifier should obviously be :ID.  If it's a
                  ;; non-Objective-C CLOS class, we're going to pass
                  ;; Objective-C objects of the LISP-VALUE-WRAPPER-MIXIN
                  ;; kind to the method, whose type specifier is :ID as
                  ;; well.
                  collect :id into type-specifiers
              else
                collect :id into type-specifiers
              finally (return
                        `(defmethod ,name
                             argtypes-start ,@type-specifiers argtypes-end
                             ,@qualifiers ,lambda-list
                           ,@body)))))))


(defmethod add-method :after ((gf objective-c-generic-function)
                              (method objective-c-method))
  #+(or) (format t "~&ADD-METHOD:~&  ~A, ~A" gf method))


(defmethod initialize-instance :around ((method objective-c-method)
                                        &rest initargs
                                        &key documentation
                                             function
                                             lambda-list
                                             qualifiers
                                             specializers
                                        &allow-other-keys)
  (declare (ignore documentation function lambda-list specializers))
  #+(or) (format t "~&INITIALIZE-INSTANCE:~&  ~S" initargs)
  (let* ((argtypes-start (position 'argtypes-start qualifiers))
         (argtypes-end (position 'argtypes-end qualifiers))
         (argument-types (subseq qualifiers (1+ argtypes-start) argtypes-end))
         (qualifiers (append (subseq qualifiers 0 argtypes-start)
                             (subseq qualifiers (1+ argtypes-end))))
         (new-initargs (copy-list initargs))
         (return-type (qualifiers-return-type qualifiers)))
    (setf (getf new-initargs :qualifiers) (remove return-type qualifiers))
    (apply #'call-next-method
           method
           :return-type (or return-type :id)
           :argument-types argument-types
           new-initargs)))


#+(or)
(defgeneric bla (x y z &rest r)
  (:generic-function-class objective-c-generic-function)
  (:method-class objective-c-method))

#+(or)
(defmethod bla :abc ((x number) (y symbol) c &rest r)
  (declare (ignore c r))
  (+ x 3))