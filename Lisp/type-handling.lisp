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


(defun parse-typespec (typestring &optional return-type-p (start 0))
  "Parse a typestring like \"@0:4{_NSRange=II}8\" into something like (ID ()).

\"rn{_NSRange=II}8\" is parsed into (STRUCT (CONST IN)
\"_NSRange\" :INTEGER :INTEGER).

\"(sversion=\"version\"i\"next_free\"^v)\" is parsed into (UNION ()
\"sversion\" (:INT ((NAME \"version\"))) (POINTER ((NAME
\"next_free\")) (:VOID ())))

Returns: (VALUES typespec byte-position string-position)"

  (let ((init-char (char typestring start))
        (string-position start)
        (qualifiers (list)))
    (loop do (setq init-char (char typestring string-position))
          while (let ((qualifier (case init-char
                                   (#\r 'const)
                                   (#\n 'in)
                                   (#\N 'inout)
                                   (#\o 'out)
                                   (#\O 'bycopy)
                                   (#\V 'oneway)
                                   (#\R 'byref)
                                   (#\" :name))))
                  (and qualifier
                       (incf string-position)
                       (if (eq qualifier :name)
                           (let ((name-end (position #\"
                                                     typestring
                                                     :start string-position)))
                             (push (list qualifier
                                         (subseq typestring
                                                 string-position
                                                 name-end))
                                   qualifiers)
                             (setf string-position (1+ name-end))
                             qualifier)
                           (push qualifier qualifiers)))))
    (let ((typespec
            (case init-char
              ((#\{ #\()
               (let* ((=-token (position #\= typestring :start start))
                      (closing-delim (position (ecase init-char
                                                 (#\{ #\})
                                                 (#\( #\)))
                                               typestring
                                               :start start))
                      (name-end (if (and =-token closing-delim)
                                    (min =-token closing-delim)
                                    ;; An opaque struct whose contents
                                    ;; we don't know.
                                    (or closing-delim
                                        (error "Premature end of file in~
                                                typespec: ~A."
                                               typestring))))
                      (named-p (and =-token (= name-end =-token)))
                      (struct-name (subseq typestring
                                           (1+ string-position)
                                           name-end)))
                 (list* (ecase init-char
                          (#\{ 'struct)
                          (#\( 'union))
                        (if named-p
                            qualifiers
                            (cons 'opaque qualifiers))
                        struct-name
                        (progn
                          (setq string-position
                                (if named-p
                                    (1+ name-end) ; skip #\=
                                    name-end))
                          (loop until (char= (char typestring string-position)
                                             (ecase init-char
                                               (#\{ #\})
                                               (#\( #\))))
                                collect (multiple-value-bind (typespec
                                                              byte-position
                                                              new-string-pos)
                                            (parse-typespec
                                             typestring
                                             nil
                                             string-position)
                                          (declare (ignore byte-position))
                                          (setq string-position new-string-pos)
                                          typespec)
                                ;; Skip end marker (right brace/paren).
                                finally (incf string-position))))))
              (#\^ (list 'pointer
                         qualifiers
                         (multiple-value-bind (typespec byte-pos new-str-pos)
                             (parse-typespec typestring
                                             nil
                                             (1+ string-position))
                           (declare (ignore byte-pos))
                           (prog1 typespec
                                  (setq string-position new-str-pos)))))
              (#\[ (list 'array
                         qualifiers
                         (multiple-value-bind (count new-str-pos)
                             (parse-integer typestring
                                            :start (1+ string-position)
                                            :junk-allowed t)
                           (prog1 count
                                  (setq string-position new-str-pos)))
                         (multiple-value-bind (typespec byte-pos new-str-pos)
                             (parse-typespec typestring nil string-position)
                           (declare (ignore byte-pos))
                           ;; Skip end marker (right bracket).
                           (prog1 typespec
                                  (setq string-position (1+ new-str-pos))))))
              (#\j
               (list 'complex
                     qualifiers
                     (multiple-value-bind (typespec byte-pos new-str-pos)
                         (parse-typespec typestring nil (1+ string-position))
                       (declare (ignore byte-pos))
                       (prog1 typespec
                              (setq string-position new-str-pos)))))
              (#\b
               (if (eq +runtime-type+ :gnu)
                   (let (bit-field-starting-pos
                         bit-field-typespec
                         bit-field-length
                         byte-position)
                     (multiple-value-setq (bit-field-starting-pos string-position)
                         (parse-integer typestring
                                        :start (1+ string-position)
                                        :junk-allowed t))
                     (multiple-value-setq (bit-field-typespec
                                           byte-position
                                           string-position)
                         (parse-typespec typestring nil string-position))
                     (multiple-value-setq (bit-field-length string-position)
                         (parse-integer typestring
                                        :start string-position
                                        :junk-allowed t))
                     (list 'bit-field
                           qualifiers
                           bit-field-starting-pos
                           bit-field-length
                           bit-field-typespec))
                   (list 'bit-field
                         qualifiers
                         nil
                         (multiple-value-bind (bit-field-length new-string-pos)
                             (parse-integer typestring
                                            :start (1+ string-position)
                                            :junk-allowed t)
                           (setq string-position new-string-pos)
                           bit-field-length))))
              (otherwise
               (let ((children (list)))
                 (prog1 (list* (case init-char
                                 (#\B :boolean) ;XXX :int?
                                 (#\c (if (and (eq +runtime-type+ :next)
                                               (or return-type-p
                                                   (featurep 'cffi-features:ppc32)))
                                          (prog1 :int
                                            (push '(nominally :char)
                                                  qualifiers))
                                          :char))
                                 (#\C (if (and (eq +runtime-type+ :next)
                                               (or return-type-p
                                                   (featurep 'cffi-features:ppc32)))
                                          (prog1 :unsigned-int
                                            (push '(nominally :unsigned-char)
                                                  qualifiers))
                                          :unsigned-char))
                                 (#\s (if (and (eq +runtime-type+ :next)
                                               (or return-type-p
                                                   (featurep 'cffi-features:ppc32)))
                                          (prog1 :int
                                            (push '(nominally :short)
                                                  qualifiers))
                                          :short))
                                 (#\S (if (and (eq +runtime-type+ :next)
                                               (or return-type-p
                                                   (featurep 'cffi-features:ppc32)))
                                          (prog1 :unsigned-int
                                            (push '(nominally :unsigned-short)
                                                  qualifiers))
                                          :unsigned-short))
                                 (#\i :int)
                                 (#\I :unsigned-int)
                                 (#\l :long)
                                 (#\L :unsigned-long)
                                 (#\q :long-long)
                                 (#\Q :unsigned-long-long)
                                 (#\f :float)
                                 (#\d :double)
                                 (#\v :void)
                                 (#\@ 'id)
                                 (#\# 'objective-c-class)
                                 (#\: 'selector)
                                 (#\* :string)
                                 (#\? :unknown)
                                 (otherwise
                                  (prog1 :unrecognised
                                    (push init-char children))))
                               qualifiers
                               children)
                        (incf string-position)))))))
      (when (and (> (length typestring) string-position)
                 (char= (char typestring string-position) #\"))
        (let ((type-end (position #\" typestring :start (1+ string-position))))
          (push (list :type (subseq typestring (1+ string-position) type-end))
                (cadr typespec))
          (setf string-position (1+ type-end))))
      (values typespec nil string-position))))


(defun typespec (typespec-designator)
  (etypecase typespec-designator
    (symbol (list typespec-designator ()))
    (list typespec-designator)))


(defun print-typespec-to-string (typespec)
  (with-output-to-string (out)
    (print-typespec typespec out)))


(defun print-typespec (typespec &optional (stream *standard-output*))
  "Convert a TYPESPEC into a typestring and write the result to a STREAM."
  (destructuring-bind (type-name modifiers &rest rest)
      (typespec typespec)
    (dolist (modifier modifiers)
      (format stream "~A" (case modifier
                            (const #\r)
                            (in #\n)
                            (inout #\N)
                            (out #\o)
                            (bycopy #\O)
                            (oneway #\V)
                            (byref #\R)
                            (opaque "")
                            (otherwise
                             (assert (listp modifier))
                             (ecase (first modifier)
                               ((nominally) (setq type-name
                                                  (second modifier))))
                             ""))))
    (case type-name
      ((struct union) (destructuring-bind (name . children) rest
                        (format stream "~C~A"
                                (ecase type-name
                                  (struct #\{)
                                  (union #\())
                                name)
                        (unless (member 'opaque modifiers)
                          (format stream "=")
                          (dolist (child children)
                            (print-typespec child stream)))
                        (format stream "~C" (ecase type-name
                                              (struct #\})
                                              (union #\))))))
      ((bit-field) (if (eq +runtime-type+ :gnu)
                       (destructuring-bind (alignment length . children) rest
                         (format stream "b~D" alignment)
                         (dolist (child children)
                           (print-typespec child stream))
                         (format stream "~D" length))
                       (destructuring-bind (alignment length . children) rest
                         (declare (ignore alignment children))
                         (format stream "b~D" length))))
      ((array) (destructuring-bind (length . children) rest
                 (format stream "[~D" length)
                 (dolist (child children)
                   (print-typespec child stream))
                 (format stream "]")))
      ((:unrecognised) (format stream "~{~A~}" rest))
      (t (format stream "~A" (typespec-name->type-id type-name))
         (dolist (child rest)
           (print-typespec child stream))))))


(defun typespec-nominal-type (typespec)
  ;; Do the modifiers include something like (NOMINALLY :UNSIGNED-CHAR)?
  ;; Return NIL if that is not the case, otherwise return the nominal
  ;; type found.
  (cadr (find-if #'(lambda (x) (and (consp x)
                                    (eq (car x) 'nominally)))
                 (cadr typespec))))


(defun typespec-primary-type (typespec)
  (car (typespec typespec)))
