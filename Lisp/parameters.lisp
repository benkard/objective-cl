(in-package #:mulk.objective-cl)


(defvar *skip-finalization*  nil)
(defvar *skip-retaining*     nil)

(defvar *trace-method-calls* nil
  "Whether to print trace messages of all Objective C method calls.

## Value Type:

a **generalized boolean**.


## Initial Value:

__nil__.


## Description:

Sometimes it is useful to find out exactly which message calls are done
in a piece of code that is executed.  If __*trace-method-calls*__ is
**true**, Objective CL tries to print trace messages to
__*terminal-io*__ that can be useful for understanding the behaviour
both of application code and the internals of Objective CL itself.

If __*trace-method-calls*__ is __nil__ (which is the default), no trace
messages are printed.

Note that there is currently no way of determining the receivers of
messages.  As this limitation severely limits the usefulness of the
trace messages, it is expected to be lifted in a future version of
Objective CL.


## Examples:

\(With __install-reader-syntax__ enabled.)

    (defvar *tmp*)
    (setq *trace-method-calls* t)

    (setq *tmp* [NSString new])
    ; Invoking [new].

    (setq *tmp* [NSString string])
    ; Invoking [string].
    ; Invoking [retain].

    (setq *tmp* nil)
    (gc :full t)
    ; Invoking [release].
    ; Invoking [release].

    (setq *trace-method-calls* nil)

\(Note that objects created by a call to `new' are not retained, because
it is the user's (that is, the Objective CL framework's) responsibility
to release them, while convenience constructors such as `string' return
objects that have already had `autorelease' called on them and must thus
be retained not to be garbage-collected prematurely.)


## Affected By:

  __*terminal-io*__


## See also:

  __invoke__, __invoke-by-name__")
