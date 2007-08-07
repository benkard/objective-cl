(in-package #:mulk.objective-cl)


(defun install-reader-syntax ()
  "Install an Objective-C-like **reader macro** for Objective C method
 calls.

## Description:

The **reader macro** installed by __install-reader-syntax__ closely
resembles the conventional method call syntax of Objective C.  In fact,
any differences between standard Objective C method call syntax and this
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

Objective C method names tend to be relatively verbose and are
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
Objective C syntax as it is whether to prefer the standard Common Lisp
__loop__ facility or a widespread and well-known alternative called
\"Iterate\".  In both cases, one can argue that one of the forms
sacrifices an elusive quality of \"lispiness\" as well as text editor
friendliness in favor of natural-language-style readability and
conciseness.


## See also:

  __invoke__, __invoke-by-name__"

  (set-macro-character #\] (get-macro-character #\)))

  (set-macro-character #\[ #'(lambda (stream char)
                               (declare (ignore char))
                               (parse-objc-call stream))))


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
                             (setf class-method-p t)
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
        (setf args (nreverse args))
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
