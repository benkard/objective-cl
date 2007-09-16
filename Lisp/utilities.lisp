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


(defun objc-typep (x class-designator)
  (objc-eql (invoke x 'class)
            (etypecase x
              (class x)
              (id (invoke x 'class))
              ((or string symbol) (find-objc-class class-designator t)))))


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


;;; (@* "Object Representation")
(defmethod print-object ((object id) stream)
  (print-unreadable-object (object stream)
    (format stream "~A `~A' {~X}"
            (objcl-class-name (primitive-invoke object "class" 'id))
            (primitive-invoke (primitive-invoke object "description" 'id)
                              "UTF8String" :string)
            (primitive-invoke object "hash" :unsigned-int))))


(defmethod print-object ((class objc-class) stream)
  (print-unreadable-object (class stream)
    (format stream "~S ~A {~X}"
            'objc-class
            (objcl-class-name class)
            (primitive-invoke class "hash" :unsigned-int))))


(defmethod print-object ((selector selector) stream)
  (print-unreadable-object (selector stream)
    (format stream "~S `~A'"
            'selector
            (selector-name selector))))


(defmethod print-object ((exception exception) stream)
  (print-unreadable-object (exception stream)
    (format stream "~S ~A {~X}"
            'exception
            (primitive-invoke (primitive-invoke exception "name" 'id)
                              "UTF8String" :string)
            (primitive-invoke exception "hash" :unsigned-int))))


;;; (@* "Structure and Union Definition")
(defun make-objc-struct/union-definer (type name-and-options c-names
                                       doc-and-slots)
  (let ((struct-name (ctypecase name-and-options
                       (list (first name-and-options))
                       (symbol name-and-options)))
        (slots (typecase (first doc-and-slots)
                 (string (rest doc-and-slots))
                 (t doc-and-slots))))
    `(progn
       ,(if (eq type :struct)
            `(defcstruct ,name-and-options ,@doc-and-slots)
            `(defcunion ,name-and-options ,@doc-and-slots))
       ,@(mapcar #'(lambda (slot)
                     (let ((slot-name (first slot)))
                       `(defun ,(intern (concatenate 'string
                                                     (symbol-name struct-name)
                                                     "-"
                                                     (symbol-name slot-name))
                                        (symbol-package slot-name))
                            (struct)
                          (check-type struct ,(if (eq type :struct)
                                                  `(or c-pointer
                                                       opaque-struct
                                                       tagged-struct)
                                                  `(or c-pointer
                                                       opaque-union
                                                       tagged-union)))
                          (when (typep struct ,(if (eq type :struct)
                                                   `'tagged-struct
                                                   `'tagged-union))
                            (assert (member (struct-name struct)
                                            ',c-names
                                            :test #'string=)))
                          (cffi:foreign-slot-value struct
                                                   ',struct-name
                                                   ',slot-name))))
                 slots))))

(defmacro define-objc-struct (name-and-options c-names &rest doc-and-slots)
  "Like CFFI:DEFCSTRUCT except that it provides accessors that check
their arguments according to their struct names."
  (make-objc-struct/union-definer :struct name-and-options c-names doc-and-slots))

(defmacro define-objc-union (name-and-options c-names &rest doc-and-slots)
  "Like CFFI:DEFCUNION except that it provides accessors that check
their arguments according to their struct names."
  (make-objc-struct/union-definer :union name-and-options c-names doc-and-slots))
