;;;; Objective-CL, an Objective-C bridge for Common Lisp.
;;;; Copyright (C) 2007  Matthias Andreas Benkard.
;;;;
;;;; This program is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

(in-package #:mulk.objective-cl)

(initialise-runtime)

(eval-when (:load-toplevel)
  (unless (boundp '+nil+)
    ;; As nil is never deallocated, we can safely use MAKE-INSTANCE
    ;; here.
    (defconstant +nil+
      (make-instance 'id :pointer (objcl-get-nil))))
  (unless (boundp '+yes+)
    (defconstant +yes+ (objcl-get-yes)))
  (unless (boundp '+no+)
    (defconstant +no+ (objcl-get-no)))
  (unless (boundp '+runtime-type+)
    (defconstant +runtime-type+ (runtime-type)))
  (pushnew (case +runtime-type+
             ((:gnu) 'objcl-features:gnu-runtime)
             ((:next) 'objcl-features:next-runtime))
           *features*))


(setf (documentation '+nil+ 'variable)
      "The Objective-C constant value `nil`.

## Value Type:

an **object** of type __id__.


## Description:

__+nil+__ is the constant corresponding to the Objective-C `nil` value.

__+nil+__ is not a value that any method invocation should return.
Whenever `nil` is returned by an Objective-C invocation, It is the job
of Objective-CL to convert it to __nil__.  Similarly, __null__ arguments
are converted to `nil` automatically.  Still, there may be occasions in
which it is useful to have `nil` as an __id__ instance.  Therefore, it
is provided here.

Note that, in the general case, `nil` is not necessarily equal to
`NULL`.")


(setf (documentation '+yes+ 'variable)
      "The Objective-C boolean value `YES`.

## Value Type:

a **number**.


## Description:

__+yes+__ is the constant corresponding to the Objective-C `YES` value.

As there is no way to distinguish methods that return booleans from
those that return numbers in the Objective-C runtime, all invocations
that ought to return booleans will actually return one of two
compile-time Objective-C constants: either `YES` or `NO`.  Lisp code
using Objective-CL needs to be aware of this and test return values
accordingly.  For this to be possible, two **constant**s are defined on
the Lisp side, analogously to Objective-C.  These are called __+yes+__
and __+no+__.


## Examples:

    (invoke (find-class 'ns-string)
            :is-subclass-of-class (find-class 'ns-object))
     ;=> #.YES

    (invoke (find-class 'ns-object)
            :is-subclass-of-class (find-class 'ns-object))
     ;=> #.YES

    (invoke (find-class 'ns-object)
            :is-subclass-of-class (find-class 'ns-string))
     ;=> #.NO


## See Also:

  __+no+__")


(setf (documentation '+no+ 'variable)
      "The Objective-C boolean value `NO`.

## Value Type:

a **number**.


## Description:

__+no+__ is the constant corresponding to the Objective-C `NO` value.

As there is no way to distinguish methods that return booleans from
those that return numbers in the Objective-C runtime, all invocations
that ought to return booleans will actually return one of two
compile-time Objective-C constants: either `YES` or `NO`.  Lisp code
using Objective-CL needs to be aware of this and test return values
accordingly.  For this to be possible, two **constant**s are defined on
the Lisp side, analogously to Objective-C.  These are called __+yes+__
and __+no+__.


## Examples:

    (invoke (find-class 'ns-string)
            :is-subclass-of-class (find-class 'ns-object))
     ;=> #.YES

    (invoke (find-class 'ns-object)
            :is-subclass-of-class (find-class 'ns-object))
     ;=> #.YES

    (invoke (find-class 'ns-object)
            :is-subclass-of-class (find-class 'ns-string))
     ;=> #.NO


## See Also:

  __+yes+__")