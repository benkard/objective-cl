(in-package #:mulk.objective-cl)


(defgeneric objc-eql (x y))
(defgeneric objc-equal (x y))


(defun truep (b)
  (not (or (zerop b)
           (null b))))


(defun id-eql (x y)
  (pointer-eq (pointer-to x) (pointer-to y)))


(defun id-equal (x y)
  (truep (if (typep x '(or id objc-class exception))
             (invoke x :is-equal y)
             (progn
               (assert (typep y '(or id objc-class exception)))
               (invoke y :is-equal x)))))


(defmethod objc-eql (x y)
  (cl:eql x y))

(defmethod objc-eql ((x id) y)
  (id-eql x y))

(defmethod objc-eql (x (y id))
  (id-eql x y))

(defmethod objc-eql ((x objc-class) y)
  (id-eql x y))

(defmethod objc-eql (x (y objc-class))
  (id-eql x y))

(defmethod objc-eql ((x exception) y)
  (id-eql x y))

(defmethod objc-eql (x (y exception))
  (id-eql x y))

(defmethod objc-eql ((x selector) (y selector))
  (eql (selector-name x) (selector-name y)))

(defmethod objc-eql ((x selector) (y string))
  (eql (selector-name x) y))

(defmethod objc-eql ((x string) (y selector))
  (eql x (selector-name y)))


(defmethod objc-equal (x y)
  (cl:equal x y))

(defmethod objc-equal ((x id) y)
  (id-equal x y))

(defmethod objc-equal (x (y id))
  (id-equal x y))

(defmethod objc-equal ((x objc-class) y)
  (id-equal x y))

(defmethod objc-equal (x (y objc-class))
  (id-equal x y))

(defmethod objc-equal ((x exception) y)
  (id-equal x y))

(defmethod objc-equal (x (y exception))
  (id-equal x y))

(defmethod objc-equal ((x selector) (y selector))
  (equal (selector-name x) (selector-name y)))

(defmethod objc-equal ((x selector) (y string))
  (equal (selector-name x) y))

(defmethod objc-equal ((x string) (y selector))
  (equal x (selector-name y)))
