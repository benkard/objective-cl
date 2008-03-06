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


(defmethod coerce-object (object (type list))
  (coerce-object object (typespec-primary-type type)))


(defcoercion :int (x)
  (coerce-object x 'integer))

(defcoercion :short (x)
  (coerce-object x 'integer))

(defcoercion :long (x)
  (coerce-object x 'integer))

(defcoercion :long-long (x)
  (coerce-object x 'integer))

(defcoercion :char (x)
  (coerce-object x 'integer))

(defcoercion :unsigned (x)
  (coerce-object x 'integer))

(defcoercion :unsigned-long (x)
  (coerce-object x 'integer))

(defcoercion :unsigned-long-long (x)
  (coerce-object x 'integer))

(defcoercion :unsigned-char (x)
  (coerce-object x 'integer))



(defcoercion :id ((x id))
  x)

(defcoercion :id ((x objective-c-class))
  x)

(defcoercion :id ((x exception))
  x)

(defcoercion :id ((x integer))
  (primitive-invoke (find-objc-class 'ns-number)
                    "numberWithInt:"
                    'id
                    x))

(defcoercion :id ((x float))
  (primitive-invoke (find-objc-class 'ns-number)
                    (etypecase x
                      (long-float "numberWithDouble:")
                      (double-float "numberWithDouble:")
                      (short-float "numberWithFloat:")
                      (single-float "numberWithFloat:"))
                    'id
                    x))

(defcoercion :id ((x null))
  +nil+)

;; (defcoercion id ((x {list, string, t})) ...): See lisp-value-wrapping.lisp.


(defcoercion :class ((x id))
  (object-get-class x))

(defcoercion :class ((x exception))
  (object-get-class x))

(defcoercion :class ((x objective-c-class))
  x)

(defcoercion :class ((x string))
  (find-objc-class x t))

(defcoercion :class ((x symbol))
  (find-objc-class x t))


(defcoercion integer ((x integer))
  x)

(defcoercion integer ((x id))
  (assert (objc-typep x 'ns-number))
  (invoke x 'int-value))

(defcoercion integer ((x number))
  (truncate x))

(defcoercion integer ((x null))
  (declare (ignore x))
  +no+)

(defcoercion integer (x)
  (declare (ignore x))
  +yes+)


(defcoercion :selector ((x selector))
  x)

(defcoercion :selector ((x symbol))
  (selector x))

(defcoercion :selector ((x string))
  (selector x))

(defcoercion :selector ((x cons))
  (selector x))


(defcoercion :exception ((x exception))
  x)


(defcoercion :char ((x null))
  0)

(defcoercion :char ((x character))
  x)

(defcoercion :char ((x integer))
  x)


(defcoercion :float ((x number))
  (float x))


(defcoercion :double ((x number))
  (float x))


;; Note that this refers to the Objective-C BOOL type, not the Lisp
;; BOOLEAN type.
(defcoercion :boolean ((x null))
  (declare (ignore x))
  +no+)

(defcoercion :boolean (x)
  (declare (ignore x))
  +yes+)


;; Note that this refers to the Lisp BOOLEAN type, not the Objective-C
;; BOOL type.
(defcoercion boolean ((x number))
  (not (zerop x)))


(defcoercion :string ((x string))
  x)

(defcoercion :string ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)


(defcoercion :pointer ((x foreign-pointer))
  (check-type x foreign-pointer)
  x)

(defcoercion :pointer ((x exception))
  (pointer-to x))

(defcoercion :pointer ((x c-pointer-wrapper))
  (pointer-to x))

(defcoercion :pointer ((x number))
  (pointer-to (coerce-object x 'id)))
