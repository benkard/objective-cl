(in-package #:mulk.objective-cl)


(defgeneric equal (x y))
(defgeneric equalp (x y))


(defun truep (b)
  (not (or (zerop b)
           (null b))))


(defun id-equal (x y)
  (declare (type (or id objc-class exception) x y))
  (truep (invoke x :is-equal y)))


(defmethod equal (x y)
  (cl:equal x y))

(defmethod equal ((x id) y)
  (id-equal x y))

(defmethod equal (x (y id))
  (id-equal x y))

(defmethod equal ((x objc-class) y)
  (id-equal x y))

(defmethod equal (x (y objc-class))
  (id-equal x y))

(defmethod equal ((x exception) y)
  (id-equal x y))

(defmethod equal (x (y exception))
  (id-equal x y))

(defmethod equal ((x selector) (y selector))
  (equal (selector-name x) (selector-name y)))

(defmethod equal ((x selector) (y string))
  (equal (selector-name x) y))

(defmethod equal ((x string) (y selector))
  (equal x (selector-name y)))


(defmethod equalp (x y)
  (cl:equalp x y))

(defmethod equalp ((x id) y)
  (equal x y))

(defmethod equalp (x (y id))
  (equal x y))

(defmethod equalp ((x objc-class) y)
  (equal x y))

(defmethod equalp (x (y objc-class))
  (equal x y))

(defmethod equalp ((x exception) y)
  (equal x y))

(defmethod equalp (x (y exception))
  (equal x y))

;; FIXME: Does this even make sense?
(defmethod equalp ((x selector) (y selector))
  (equalp (selector-name x) (selector-name y)))

(defmethod equalp ((x selector) (y string))
  (equalp (selector-name x) y))

(defmethod equalp ((x string) (y selector))
  (equalp x (selector-name y)))
