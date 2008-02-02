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
                                   (#\R 'byref))))
                  (and qualifier
                       (incf string-position)
                       (push qualifier qualifiers))))
    (values (case init-char
              ((#\{ #\()
               (let* ((=-token (position #\= typestring :start start))
                      (name-end (or =-token
                                    ;; An opaque struct whose contents
                                    ;; we don't know.
                                    (position (ecase init-char
                                                (#\{ #\})
                                                (#\( #\)))
                                              typestring
                                              :start start)
                                    (error "Premature end of file in~
                                            typespec: ~A."
                                           typestring)))
                      (struct-name (subseq typestring
                                           (1+ string-position)
                                           name-end)))
                 (list* (ecase init-char
                          (#\{ 'struct)
                          (#\( 'union))
                        (if =-token
                            qualifiers
                            (cons 'opaque qualifiers))
                        struct-name
                        (progn
                          (setq string-position
                                (if =-token
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
                       bit-field-typespec)))
              (otherwise
               (prog1 (list (case init-char
                              (#\B :boolean) ;XXX :int?
                              (#\c (if (and (eq +runtime-type+ :next)
                                            (or return-type-p
                                                (featurep 'cffi-features:ppc32)))
                                       :int
                                       :char))
                              (#\C (if (and (eq +runtime-type+ :next)
                                            (or return-type-p
                                                (featurep 'cffi-features:ppc32)))
                                       :unsigned-int
                                       :unsigned-char))
                              (#\s (if (and (eq +runtime-type+ :next)
                                            (or return-type-p
                                                (featurep 'cffi-features:ppc32)))
                                       :int
                                       :short))
                              (#\S (if (and (eq +runtime-type+ :next)
                                            (or return-type-p
                                                (featurep 'cffi-features:ppc32)))
                                       :unsigned-int
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
                              (#\? :unknown))
                            qualifiers)
                      (incf string-position))))
            #+(or)  ; too greedy (=> bit-fields can't see their length!)
            (multiple-value-bind (byte-position new-string-pos)
                (parse-integer typestring
                               :start string-position
                               :junk-allowed t)
              (setq string-position new-string-pos)
              byte-position)
            #-(or) nil
            string-position)))