#lang scribble/manual

@title{stylish}
@require[(for-label mischief/stylish)]
@defmodule[mischief/stylish]

@defproc[
(stylish-print [x any/c] [port output-port? (current-output-port)]
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
(stylish-println [x any/c] [port output-port? (current-output-port)]
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
