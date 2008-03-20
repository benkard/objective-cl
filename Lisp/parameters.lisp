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


(defvar *runtime-initialisation-level* 0)

(defvar *object-locks* (make-hash-table :test 'eql))

(defvar *boolean-return-exceptions* (make-hash-table :test #'equal))

(defvar *skip-retaining* nil)

(defvar *in-make-pointer-wrapper-p* nil
  "A debugging tool that helps identify direct MAKE-INSTANCE calls that
ought not be there.")

(defvar *trace-method-calls* nil
  "Whether to print trace messages of all Objective-C method calls.

## Value Type:

a **generalized boolean**.


## Initial Value:

__nil__.


## Description:

Sometimes it is useful to find out exactly which message calls are done
in a piece of code that is executed.  If __*trace-method-calls*__ is
**true**, Objective-CL tries to print trace messages to
__*terminal-io*__ that can be useful for understanding the behaviour
both of application code and the internals of Objective-CL itself.

If __*trace-method-calls*__ is __nil__ (which is the default), no trace
messages are printed.

Note that there is currently no way of determining the receivers of
messages.  As this limitation severely limits the usefulness of the
trace messages, it is expected to be lifted in a future version of
Objective-CL.


## Examples:

\(With __install-reader-syntax__ enabled.)

    (defvar \\*tmp\\*)
    (setq \\*trace-method-calls\\* t)

    (setq \\*tmp\\* [NSString new])
    ; Invoking [new].

    (setq \\*tmp\\* [NSString string])
    ; Invoking [string].
    ; Invoking [retain].

    (setq \\*tmp\\* nil)
    (gc :full t)
    ; Invoking [release].
    ; Invoking [release].

    (setq *trace-method-calls* nil)

\(Note that objects created by a call to `new' are not retained, because
it is the user's (that is, the Objective-CL framework's) responsibility
to release them, while convenience constructors such as `string' return
objects that have already had `autorelease' called on them and must thus
be retained not to be garbage-collected prematurely.)


## Affected By:

  __*terminal-io*__


## See also:

  __invoke__, __invoke-by-name__")
