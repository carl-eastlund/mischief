#lang scribble/manual

@title{stylish}
@require[(for-label mischief/stylish)]
@defmodule[mischief/stylish]

@defproc[
(stylish-print [x any/c]
  [port output-port? (current-output-port)]
  [#:expr-style est expr-style? (current-expr-style)]
  [#:print-style pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
void?
]{
Prints an expression representing @racket[x] to @racket[port].  Converts
@racket[x] to an expression according to @racket[est] and renders the
expression as text according to @racket[pst].  Formats the result to fit in
@racket[cols] columns, assuming the output is preceded by @racket[left]
characters on the first line and will be followed by @racket[right] characters
on the final line.
}

@defproc[
(stylish-println [x any/c]
  [port output-port? (current-output-port)]
  [#:expr-style est expr-style? (current-expr-style)]
  [#:print-style pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
void?
]{
Prints an expression representing @racket[x] to @racket[port] followed by a
newline.  Equivalent to calling @racket[stylish-print] followed by
@racket[(newline port)].
}

@defproc[(stylish-print-handler [x any/c]) void?]{
Intended for use with @racket[current-print].  Calls
@racket[(stylish-println x)] unless @racket[x] is @racket[(void)].
}

@defproc[
(stylish-value->string [x any/c]
  [#:expr-style est expr-style? (current-expr-style)]
  [#:print-style pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
string?
]{
Renders an expression representing @racket[x] as a string.  Equivalent to
combining @racket[get-output-string], @racket[stylish-print], and
@racket[open-output-string].
}

@defproc[
(stylish-print-expr [x any/c]
  [port output-port? (current-output-port)]
  [pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
void?
]{
Prints @racket[x] to @racket[port].  As @racket[stylish-print],
assuming @racket[x] has already been converted to an expression.
}

@defproc[
(stylish-println-expr [x any/c]
  [port output-port? (current-output-port)]
  [pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
void?
]{
Prints @racket[x] to @racket[port], followed by a newline.  As
@racket[stylish-println], assuming @racket[x] has already been converted to an
expression.
}

@defproc[
(stylish-expr->string [x any/c]
  [pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
string?
]{
Renders @racket[x] as a string.  As @racket[stylish-value->string], assuming
@racket[x] has already been converted to an expression.
}

@defproc[
(stylish-value->expr [x any/c]
  [est expr-style? (current-expr-style)])
any/c
]{
Converts the value @racket[x] to an expression according to @racket[est].
}

@defproc[
(stylish-quotable-value? [x any/c]
  [est expr-style? (current-expr-style)])
boolean?
]{
Reports whether @racket[x] can be @racket[quote]d to convert it to an
expression according to @racket[est].
}

@defproc[
(stylish-printf [fmt string?] [arg any/c] ...
  [#:port port output-port? (current-output-port)]
  [#:expr-style est expr-style? (current-expr-style)]
  [#:print-style pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
void?
]{
Similar to @racket[fprintf], prints @racket[fmt], substituting a printed
representation of each @racket[arg] for formatting sequences.  The sequence
@racketvalfont{~a} prints an @racket[arg] using @racket[display];
@racketvalfont{~s} uses @racket[stylish-print-expr]; and @racketvalfont{~v}
uses @racket[stylish-print].  The sequence @racketvalfont{~f} expects the
corresponding arg to have the form @racket[(list fmt arg ...)], and prints
recursively using @racket[stylish-printf].
}

@defproc[
(stylish-format [fmt string?] [arg any/c] ...
  [#:expr-style est expr-style? (current-expr-style)]
  [#:print-style pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
string?
]{
In the same way that @racket[format] renders the output of @racket[fprintf] as
a string, @racket[stylish-format] renders the output of @racket[stylish-printf]
as a string.
}

@defproc[
(call-with-stylish-port
  [port output-port?]
  [proc (-> output-port? any)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
any
]{
Used for recursive stylish-printing; calls @racket[proc] with an output port
that records the structure of printed values, breaks lines and indents them
appropriately, and prints the final rendered form on @racket[port] when
@racket[proc] returns.
}

@defproc[
(stylish-print-separator
  [port output-port?]
  [#:indent indent exact-nonnegative-integer? 0]
  [#:wide? wide? boolean? #t])
void?
]{
Prints whitespace to @racket[port].  If @racket[port] is created by
@racket[call-with-stylish-port], then the separator may be used to break lines
by printing a newline and indenting to @racket[indent] spaces beyond the start
of the current line at the current level of recursive stylish-printing.
Otherwise, the separator prints a single space if @racket[wide?] is true and
prints nothing if @racket[wide?] is false.
}

@defparam[
current-stylish-print-columns
cols
(or/c exact-nonnegative-integer? 'infinity)
]{
Controls the number of columns used for breaking lines and indenting by
@racket[stylish-print] and related procedures, when no explicit number of
columns is given.
}

@defproc[(print-style? [x any/c]) boolean?]{
Recognizes print styles, which control the way stylish-printing renders
values that represent expressions as text.
}

@defthing[empty-print-style print-style?]{
A degenerate style that will not print; extend this to build new print styles
from scratch.
}

@defthing[simple-print-style print-style?]{
A basic print style that uses @racket[write] for all values.
}

@defthing[default-print-style print-style?]{
A print style that renders most readable Racket types in a form that will
@racket[read] as the same thing.
}

@defproc[(print-style-extension? [x any/c]) boolean?]{
Recognizes print style extensions, which control the way stylish-printing
renders some specific type of values representing expressions as text.
}

@defproc[
(print-style-extension
  [pred predicate/c]
  [proc (-> pred output-port? print-style? void?)])
print-style-extension?
]{
Creates a print style extension that prints values satisfying @racket[pred]
using @racket[proc], given the current output port and print style.
}

@defparam[current-print-style pst print-style?]{
Controls the print style used by @racket[stylish-print] and related procedures
if none is given explicitly.  Defaults to @racket[default-print-style].
}

@defform[(with-stylish-port body ...+)]{
Equivalent to:
@(racketblock
   (call-with-stylish-port (current-output-port)
     (lambda (port)
       (parameterize {[current-output-port port]}
         body ...))))
}

@defstruct*[stylish-comment-expr ((comment string?) (expr any/c))]{
Represents an @racket[expr]ession annotated with a @racket[comment].
In @racket[default-print-style], prints as
@racket[expr]@racketfont{ #|}@racket[comment]@racketfont{|#}.
}

@defstruct*[stylish-unprintable-expr ((name symbol?))]{
Represents a value without a readable/writable representation.
In @racket[default-print-style], prints as
@racketfont{#<}@racket[name]@racketfont{>}.
}
