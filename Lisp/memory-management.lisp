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


(defvar *id-objects* (make-weak-value-hash-table))
(defvar *class-objects* (make-weak-value-hash-table))
(defvar *exception-objects* (make-weak-value-hash-table))
(defvar *selector-objects* (make-weak-value-hash-table))
(defvar *meta-class-objects* (make-weak-value-hash-table))


(defun make-pointer-wrapper (class &rest initargs &key pointer &allow-other-keys)
  (let* ((hash-table (ecase class
                       ((id) *id-objects*)
                       ((objc-class) *class-objects*)
                       ((exception) *exception-objects*)
                       ((selector) *selector-objects*)
                       ((objc-meta-class) *meta-class-objects*)))
         (address (cffi:pointer-address pointer))
         (object (weak-gethash address hash-table nil))
         (constructor (case class
                        ((exception) #'make-condition)
                        (otherwise #'make-instance))))
    (if object
        object
        (progn
          ;; Note that we do not care whether another thread does the
          ;; same here, so we don't need to lock the hash table before
          ;; peeking into it.  If our new object isn't put into the hash
          ;; table because another thread was faster than us, that's
          ;; fine.  The important thing here is that (a) all objects
          ;; that do get into the hash table are properly set up for
          ;; garbage collection, and (b) most objects don't need to be
          ;; boxed and set up for garbage collection (and later
          ;; garbage-collected) anew all the time but can be retrieved
          ;; from the hash table.
          ;;
          ;; (a) is ensured by MAKE-INSTANCE (see below), while (b) is
          ;; what this function is all about.
          ;;
          ;; Note, too, that we would indeed have to lock the hash table
          ;; before peeking into it if we wanted all wrapper objects to
          ;; the same object to be EQL.  I think that that would
          ;; probably not only be necessary, but even sufficient.
          ;;
          ;; By the way, is using the return value of SETF considered
          ;; bad style?
          (let* ((*in-make-pointer-wrapper-p* t)
                 (new-wrapper (apply constructor class initargs)))
            (setf (weak-gethash address hash-table) new-wrapper)
            ;; As classes always have a retain count of -1, we don't
            ;; have to do memory management for them.  Meanwhile,
            ;; selectors and meta-classes cannot receive messages, so
            ;; trying to do memory management for them would not be
            ;; healthy.  Considering these facts, doing memory
            ;; management only for id instances seems the right thing to
            ;; do.
            (when (eq class 'id)
              ;; We call the `retain' method on every object that we
              ;; receive from a method call or otherwise except
              ;; non-convenience constructor methods (i.e. those whose
              ;; name starts with `alloc' or `new').  Upon Lisp-side
              ;; finalization of an object, wie `release' it.
              (unless *skip-retaining*
                (primitive-invoke new-wrapper "retain" 'id))
              (flet ((finalizer ()
                       ;; In order to send the `release' message to the
                       ;; newly GC'd object, we have to create a
                       ;; temporary container object for the final
                       ;; message delivery.  Note that this can cause an
                       ;; infinite recursion or even memory corruption
                       ;; if we don't take measure to skip both
                       ;; finalization and retaining of the temporary
                       ;; object.  Therefore, we call MAKE-INSTANCE
                       ;; directly.
                       ;;
                       ;; (In principle, PRIMITIVE-INVOKE should also
                       ;; happily take a pointer as its first argument,
                       ;; but why push our luck?)
                       (let* ((temporary-wrapper
                               (funcall constructor class :pointer pointer)))
                         (primitive-invoke temporary-wrapper "release" :void))))
                (trivial-garbage:finalize new-wrapper #'finalizer)))
            new-wrapper)))))
