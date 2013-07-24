#lang scribble/manual

@(require mischief/examples)

@title[#:tag "stylish"]{@racketmodname[mischief/stylish]: Stylish Printing, an Alternative to Pretty Printing}
@require[(for-label mischief/stylish)]
@defmodule[mischief/stylish]

The @racketmodname[mischief/stylish] collection defines "stylish" printing as
an alternative to "pretty" printing from @racketmodname[racket/pretty].
Stylish printing uses more uniform formatting by default, is easier to
customize, and supports @racket[printf]-style formatted printing.

@examples/evaluator[mischief
(define x
  (list
    (list 1 2 3)
    (vector 4 5 6)
    (quote-syntax (7 8 9))))
(stylish-printf #:columns 20
  "We can print inputs as values [~v] and expressions [~s]."
  x
  x)
(stylish-print x #:columns 20)
(stylish-write x #:columns 20)
(stylish-value->expr x)
]

@section{Rendering Values Converted to Expressions}

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

@defproc[
(stylish-print-as-string [x any/c]
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

@defproc[(stylish-print-handler [x any/c]) void?]{
Intended for use with @racket[current-print].  Calls
@racket[(stylish-println x)] unless @racket[x] is @racket[(void)].
}

@section{Rendering Values Without Conversion}

@defproc[
(stylish-write [x any/c]
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
(stylish-writeln [x any/c]
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
(stylish-write-as-string [x any/c]
  [pst print-style? (current-print-style)]
  [#:left left exact-nonnegative-integer? 0]
  [#:right right exact-nonnegative-integer? 0]
  [#:columns cols 
             (or/c exact-nonnegative-integer? 'infinity)
             (current-stylish-print-columns)])
string?
]{
Renders @racket[x] as a string.  As @racket[stylish-print-as-string], assuming
@racket[x] has already been converted to an expression.
}

@section{Converting Values to Expressions}

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

@section{Rendering Values Using Format Strings}

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
@racketvalfont{~s} uses @racket[stylish-write]; and @racketvalfont{~v}
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

@section{Custom Stylish Printing}

@defparam[
current-stylish-print-columns
cols
(or/c exact-nonnegative-integer? 'infinity)
]{
Controls the number of columns used for breaking lines and indenting by
@racket[stylish-print] and related procedures, when no explicit number of
columns is given.
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

@subsection{Print Styles}

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

@defproc[
(extend-print-style [pst print-style?] [ext print-style-extension?] ...
  [#:after? after? boolean? #false])
print-style?
]{
Adds the given extensions to @racket[pst].  Checks the new extensions before
any existing extensions, unless @racket[after?] is true.
}

@defproc[
(set-print-style-default-printer
  [pst print-style?]
  [proc (or/c (-> any/c output-port? void?) #false)])
print-style?
]{
Updates @racket[pst] to use @racket[proc] to print any value that has no
specific extension.
}

@defparam[current-print-style pst print-style?]{
Controls the print style used by @racket[stylish-print] and related procedures
if none is given explicitly.  Defaults to @racket[default-print-style].
}

@subsection{Expression Styles}

@defproc[(expr-style? [x any/c]) boolean?]{
Recognizes expression styles, which control the way values are converted to
expressions.
}

@defthing[empty-expr-style expr-style?]{
A degenerate expression style that cannot convert any values; extend this to
build new expression styles from scratch.
}

@require[(for-label mzlib/pconvert)]
@defthing[simple-expr-style expr-style?]{
A basic expression style that uses @racket[print-convert] for all values.
}

@defthing[default-expr-style expr-style?]{
An expression style that converts most built-in Racket types to an expression
that will @racket[eval] to the same thing.
}

@defproc[(expr-style-extension? [x any/c]) boolean?]{
Recognizes expression style extensions, which control the way that specific
types of values are converted to expressions.
}

@defproc[
(expr-style-extension
  [pred predicate/c]
  [convert (-> pred expr-style? any/c)]
  [quotable? (-> pred expr-style? boolean?) (lambda {x est} #false)]
  [try-quote? boolean? #true])
expr-style-extension?
]{
Produces an extension that converts values satisfying @racket[pred] to
expressions using @racket[convert], given the current expression style.  The
predicate @racket[quotable?] controls whether values of this type can be
included in @racket[quote] expressions; generally opaque values such as
procedures should not be considered quotable.  The flag @racket[try-quote?]
controls whether the conversion function prefers to use @racket[quote] for
values of this type; generally self-quoting values can use @racket[#false] for
@racket[try-quote?].
}

@defproc[
(extend-expr-style [est expr-style?] [ext expr-style-extension?] ...
  [#:after? after? boolean? #false])
expr-style?
]{
Adds the given extensions to @racket[est].  Checks the new extensions before
any existing extensions, unless @racket[after?] is true.
}

@defproc[
(set-expr-style-default-convert
  [est expr-style?]
  [proc (or/c (-> any/c any/c) #false)])
expr-style?
]{
Updates @racket[est] to use @racket[proc] to convert any value that has no
specific extension.
}

@defparam[current-expr-style pst expr-style?]{
Controls the expression style used by @racket[stylish-expr] and related
procedures if none is given explicitly.  Defaults to
@racket[default-expr-style].
}

@defform[(with-stylish-port body ...+)]{
Equivalent to:
@(racketblock
   (call-with-stylish-port (current-output-port)
     (lambda (port)
       (parameterize {[current-output-port port]}
         body ...))))
}

@subsection{Generic Stylish Printing}

@defidform[gen:stylish-printable]{

A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic interface}
containing the methods @racket[generic-stylish-quotable?] and
@racket[generic-stylish-value->expr].

}

@defproc[(stylish-printable? [x any/c]) boolean?]{

Returns @racket[#true] if @racket[x] is an instance of
@racket[gen:stylish-printable]; returns @racket[@false] otherwise.

}

@defproc[(generic-stylish-quotable? [x stylish-printable?]) boolean?]{

Used by @racket[default-print-style] to compute the result of
@racket[stylish-quotable-value?] for instances of
@racket[gen:stylish-printable].

}

@defproc[(generic-stylish-value->expr [x stylish-printable?]) boolean?]{

Used by @racket[default-print-style] to compute the result of
@racket[stylish-value->expr] for instances of
@racket[gen:stylish-printable].

}

@defidform[gen:stylish-writable]{

A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic interface}
containing the method @racket[generic-stylish-write].

}

@defproc[(stylish-writable? [x any/c]) boolean?]{

Returns @racket[#true] if @racket[x] is an instance of
@racket[gen:stylish-writable]; returns @racket[@false] otherwise.

}

@defproc[(generic-stylish-write [x stylish-printable?]) boolean?]{

Used by @racket[default-expr-style] to perform @racket[stylish-write] for
instances of @racket[gen:stylish-writable].

}

@subsection{Special Expression Types}

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
