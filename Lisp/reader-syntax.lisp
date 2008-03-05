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


(defvar *method-syntax-macro-chars* (list))
(defvar *bracket-syntax-macro-chars* (list))
(defvar *readtable-stack* (list))


(defun restore-readtable ()
  (when *readtable-stack*
    (setq *readtable* (pop *readtable-stack*)))
  (values))


(defun save-readtable ()
  (push *readtable* *readtable-stack*)
  (setq *readtable* (copy-readtable *readtable*))
  (values))


(defun enable-method-syntax ()
  "Install a **reader macro** that makes method calls look nicer.

## Description:

The **reader macro** installed by __enable-method-syntax__ makes it
easier to write method invocations as well as making them more readable
alongside Lisp code by placing the method name in front.  At the same
time, it is a more conservative syntax enhancement than that provided by
__enable-objective-c-syntax__.

The **reader macro** transforms any **string** of alphanumeric
characters and **character**s that are __eql__ to one of #\:, #\- and
#\_ into a **symbol** with that **string** as the **symbol name** and
_objective-c-selectors__ as the **symbol package**.


## Examples:

    #.(enable-method-syntax)

    (#/stringWithCString:encoding: \"Hi there!\" 0)
      => 

    (defvar *lock* (#/new (find-objc-class 'ns-lock)))
      => *LOCK*

    (#/lock lock)
    (#/tryLock lock)
    (#/unlock lock)

    #.(disable-method-syntax)


## Note:

Absent manual changes by the user, the __fdefinition__ of any **fbound**
**symbol** read by this **reader macro** will be a __selector__.

Method __selector__s have to be interned prior to use.  As this reader
macro is not capable of interning new __selector__s, you have to ensure
that __intern-selector__ is called before the respective __selector__ is
used.  This is not a problem for __selector__s known at load-time nor for
__selector__s registered by way of __collect-selectors__.


## See also:

  __enable-objective-c-syntax__"

  (save-readtable)
  (push (get-dispatch-macro-character #\# #\/) *method-syntax-macro-chars*)
  (set-dispatch-macro-character #\# #\/ #'(lambda (stream char arg)
                                            (declare (ignore char arg))
                                            (read-objective-c-method stream)))
  (values))


(defun disable-method-syntax ()
  "FIXME"
  (restore-readtable)
  #+(or) (when *method-syntax-macro-chars*
           (let ((macro-char (pop *method-syntax-macro-chars*)))
             (when macro-char
               (set-dispatch-macro-character #\# #\/ macro-char))))
  (values))


(defun read-objective-c-method (stream)
  (loop for char = (read-char stream nil nil t)
        while (or (alphanumericp char)
                  (member char '(#\: #\- #\_)))
        collect char into constituents
        finally (progn
                  (when char (unread-char char stream))
                  (let ((symbol (intern (format nil "~{~A~}" constituents)
                                        '#:objective-c-selectors)))
                    (return symbol)))))


(defun install-reader-syntax ()
  "This function is deprecated.  Use __enable-objective-c-syntax__ instead."
  (enable-objective-c-syntax))


(defun enable-objective-c-syntax ()
  "Install an Objective-C-like **reader macro** for Objective-C method
 calls.

## Description:

The **reader macro** installed by __enable-objective-c-syntax__ closely
resembles the conventional method call syntax of Objective-C.  In fact,
any differences between standard Objective-C method call syntax and this
**reader macro** that are not documented here are probably bugs and
should be reported to the maintainer.


## Reader Macro:

[\\{*receiver* | *class-name*\\} *message-name-part [[argument [[message-name-arg-pair]]*\\**]]*]

message-name-arg-pair ::= *message-name-part argument*


## Reader Macro Arguments and Values:

*receiver* --- an **object**.

*class-name* --- a **symbol** (not evaluated).

*message-name-part* --- a **symbol** (not evaluated).

*argument* --- an **object**.

Returns: *result* --- the result of the method invocation.


## Reader Macro Description:

First, it is determined whether to regard the first element of the
form as an object or as a class name according to the following rules:

1. If the element is no **symbol**, it is regarded as an object.

2. Otherwise, if it is a **symbol** whose **name** starts with a
**lowercase** letter, it is regarded as an object.

3. Otherwise, it is regarded as a class name.

Next, the method call is parsed by alternately
reading *message name parts* and *arguments* separated by
whitespace.  *Message name parts* are expected to consist of
**alphanumeric** **character**s and **colon**s, while *arguments* may
be arbitrary Lisp expressions.  If any but the first *message name part*
does not end with a **colon**, the effects are undefined.  Likewise, the
effects are undefined if any *message name part* contains a **colon** in
a position other than the end.

After the parsing is done, all *message name parts* are concatenated in
order to form a single **string** that is used as if as the second
**argument** to __invoke-by-name__, and all *arguments* are collected in
order and the resulting **list** used as if as a **list** of additional
arguments to __invoke-by-name__.


## Reader Macro Examples:

    [NSString stringWithCString: \"Mulk.\"]
      ;=> #<GSCBufferString `Mulk.' {5B36087}>

    [NSObject self]
      ;=> #<NSObject `NSObject' {16ECF598}>

    [NSObject name]
      ;=> \"NSObject\"

    [[[NSObject self] self] name]
      ;=> \"NSObject\"

    [NSString stringWithCString: \"Mulk.\" encoding: 4]
      ;=> #<GSCBufferString `Mulk.' {5B36087}>



## Rationale:

Objective-C method names tend to be relatively verbose and are
frequently composed of many short words like \"of\" and \"by\".  As a
result, using __invoke__ can be quite cumbersome at times and waste
screen real estate.  One need only compare the example call

    (invoke (find-objc-class 'ns-string)
            :string-with-c-string \"Mulk.\"
            :encoding 4)

with its reader macro counterpart

    [NSString stringWithCString: \"Mulk.\" encoding: 4]

to be able to see an improvement in length as well as readability.

In any case, it is a matter of taste whether to prefer __invoke__ or
Objective-C syntax as it is whether to prefer the standard Common Lisp
__loop__ facility or a widespread and well-known alternative called
\"Iterate\".  In both cases, one can argue that one of the forms
sacrifices an elusive quality of \"lispiness\" as well as text editor
friendliness in favor of natural-language-style readability and
conciseness.


## See also:

  __invoke__, __invoke-by-name__, __disable-objective-c-syntax__,
__enable-method-syntax__"

  (push (cons (get-macro-character #\[)
              (get-macro-character #\]))
        *bracket-syntax-macro-chars*)
  (save-readtable)
  (set-macro-character #\] (get-macro-character #\)))
  (set-macro-character #\[ #'(lambda (stream char)
                               (declare (ignore char))
                               (parse-objc-call stream)))
  (values))


(defun disable-objective-c-syntax ()
  "FIXME"
  (restore-readtable)
  #+(or) (when *bracket-syntax-macro-chars*
           (destructuring-bind (open . close)
               (pop *bracket-syntax-macro-chars*)
             (when open
               (set-macro-character #\[ open))
             (when close
               (set-macro-character #\] close))))
  (values))


(defun parse-objc-call (stream)
  (let ((*standard-input* stream))
    (flet ((read-message-part (buffer)
             (do ((char (read-char stream t nil t)
                        (read-char stream t nil t)))
                 ((not (or (alphanumericp char)
                           (member char (coerce ":_-" 'list))))
                  (unread-char char))
               (vector-push-extend char buffer)))
           (slurp-whitespace ()
             (do ((char nil
                        (read-char stream t nil t)))
                 ((not (member (peek-char) '(#\Space #\Newline #\Tab)))))))
      (let* ((class-method-p nil)
             (receiver (if (upper-case-p (peek-char))
                           ;; A class name.
                           (let ((*readtable* (copy-readtable)))
                             (setq class-method-p t)
                             (setf (readtable-case *readtable*) :preserve)
                             `(find-objc-class
                               ,(symbol-name (read stream t nil t))))
                           ;; Something else.
                           (read stream t nil t)))
             (args (list))
             (message (make-array '(0) :element-type 'character
                                       :adjustable t :fill-pointer t)))

        (slurp-whitespace)
        (do ()
            ((char= #\] (peek-char)))
          (read-message-part message)
          (slurp-whitespace)
          (unless (char= #\] (peek-char))
            (push (read stream t nil t) args)
            (slurp-whitespace)))

        ;; Slurp the trailing #\].
        (assert (char= #\] (read-char)))
        (setq args (nreverse args))
        `(,(if class-method-p
               'invoke-by-name
               #+nil 'objcl-invoke-instance-method
               #-nil 'invoke-by-name)
           ,receiver
           ,(make-array (list (length message))
                        :element-type 'character
                        :initial-contents message
                        :adjustable nil
                        :fill-pointer nil)
           ,@args)))))
