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


(defun define-returns-boolean-exception (selector-designator)
  "Define an exception to the rule that `char` means `BOOL` as a method return type.

## Arguments and Values:

*selector-designator* --- a *selector designator*.


## Description:

Normally, Objective-C treats method return values that are nominally of
type `char` as booleans and converts them to either __t__ or __nil__
depending on whether they are __zerop__.
__define-returns-boolean-exception__ directs Objective-CL to treat
`char` values returned by methods named by *selector-designator* as
numbers instead.

__undefine-returns-boolean-exception__ restores the default behaviour.


## Examples:

    (define-returns-boolean-exception \"charValue\")
    (define-returns-boolean-exception \"characterAtIndex:\")


## Rationale:

The Objective-C runtime offers no way of distinguishing booleans from
chars, even though Foundation defines a `BOOL` type.  In the vast
majority of cases, `char` therefore actually means `BOOL`, but the
`NSString` class, for one, wouldn't always agree with that sentiment, so
the only sane way of handling both booleans and actual `char` values is
to determine the intentional type by method selector.


## See also:

  __undefine-returns-boolean-exception__"
  (let ((key (typecase selector-designator
               (string selector-designator)
               (t (selector-name (selector selector-designator))))))
    (setf (gethash key *boolean-return-exceptions*) t)))


;; FIXME: Document.
(defun undefine-returns-boolean-exception (selector-designator)
  "Revert the effect of __define-returns-boolean-exception__ for a given selector.

## Arguments and Values:

*selector-designator* --- a *selector designator*.


## Description:

Normally, Objective-C treats method return values that are nominally of
type `char` as booleans and converts them to either __t__ or __nil__
depending on whether they are __zerop__.
__define-returns-boolean-exception__ directs Objective-CL to treat
`char` values returned by methods named by *selector-designator* as
numbers instead.

__undefine-returns-boolean-exception__ restores the default behaviour.


## Examples:

    (undefine-returns-boolean-exception \"boolValue\")
    (undefine-returns-boolean-exception \"isEqual:\")


## Rationale:

The Objective-C runtime offers no way of distinguishing booleans from
chars, even though Foundation defines a `BOOL` type.  In the vast
majority of cases, `char` therefore actually means `BOOL`, but the
`NSString` class, for one, wouldn't always agree with that sentiment, so
the only sane way of handling both booleans and actual `char` values is
to determine the intentional type by method selector.


## See also:

  __define-returns-boolean-exception__"
  (let ((key (typecase selector-designator
               (string selector-designator)
               (t (selector-name (selector selector-designator))))))
    (remhash key *boolean-return-exceptions*)))


(define-returns-boolean-exception "charValue")
(define-returns-boolean-exception "characterAtIndex:")


(defun returned-char-is-bool-p (receiver selector)
  (declare (ignore receiver))
  (gethash (selector-name selector) *boolean-return-exceptions* nil))


(defun objc-char->lisp-value (objc-char char-is-bool-p)
  (if char-is-bool-p
      objc-char
      (not (zerop objc-char))))
