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


#+sbcl
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (find-objc-class "NSArray" t))

  (defmethod sb-sequence:length ((array ns::ns-array))
    (invoke-by-name array "count"))

  (defmethod sb-sequence:elt ((array ns::ns-array) index)
    (invoke-by-name array "objectAtIndex:" index))

  (defmethod (setf sb-sequence:elt) (new-value (array ns::ns-array) index)
    (invoke-by-name array "setObject:atIndex:" new-value index))

  (defmethod sb-sequence:adjust-sequence ((array ns::ns-array)
                                          length
                                          &key initial-element
                                               initial-contents)
    (cond ((< length (sb-sequence:length array))
           (dotimes (i (- (sb-sequence:length array) length))
             (invoke-by-name array "removeLastObject")))
          ((> length (sb-sequence:length array))
           (loop for i from (sb-sequence:length array) below length
                 do (invoke-by-name array
                                    "addObject:"
                                    (if (> (length initial-contents) i)
                                        (elt initial-contents i)
                                        initial-element)))))
    array)

  (defmethod sb-sequence:make-sequence-like ((array ns::ns-array)
                                             length
                                             &key initial-element
                                                  initial-contents)
    (let ((new-array (invoke-by-name (class-of array)
                                     "arrayWithArray:"
                                     array)))
      (sb-sequence:adjust-sequence new-array
                                   length
                                   :initial-element initial-element
                                   :initial-contents initial-contents))))
