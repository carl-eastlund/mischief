#lang scribble/manual

@(require mischief/examples scribble/eval)

@title[#:style '(toc)]{Printing and Error Messages}

@include-section["stylish.scrbl"]

@section[#:tag "phrase"]{@racketmodname[mischief/phrase]: Converting Values to Readable Phrases}
@require[(for-label mischief/phrase)]
@defmodule[mischief/phrase]

@(define-example-evaluator phrase-eval mischief)

@defproc*[(
[(procedure-arity->phrase [proc procedure?]) string?]
[(procedure-arity->phrase [arity procedure-arity?]) string?]
[(procedure-arity->phrase
   [arity procedure-arity?]
   [required (listof keyword?)]
   [optional (or/c (listof keyword?) #false)])
 string?]
)]{
Produces a readable phrase describing the positional and keyword arity of a
procedure.

@examples[#:eval phrase-eval
(procedure-arity->phrase sort)
(procedure-arity->phrase (list 1 2 3 5 (arity-at-least 8)))
(procedure-arity->phrase 0 '[] #false)
(procedure-arity->phrase 1 '[#:always] '[#:always #:sometimes])
(procedure-arity->phrase 2
  '[#:always #:usually]
  '[#:sometimes #:occasionally])
]
}

@defproc[
(list->phrase [strs (listof string?)]
  [#:none none string? "none"]
  [#:separator sep string? ","]
  [#:final final (or/c string? #false) "and"])
string?
]{
Produces a readable phrase describing a list of things.

@examples[#:eval phrase-eval
(list->phrase empty)
(list->phrase empty #:none "nothing at all")
(list->phrase '["apples"])
(list->phrase '["apples" "bananas"])
(list->phrase '["apples" "bananas" "oranges"])
(list->phrase '["apples and bananas" "carrots and celery" "meat and potatoes"]
  #:separator ";"
  #:final "or")
]
}

@defproc[
(count->phrase
  [n exact-nonnegative-integer?]
  [singular string?]
  [plural (or/c string? #false) #false])
string?
]{
Produces a readable phrase describing the quantity of something.

@examples[#:eval phrase-eval
(count->phrase 0 "elephant")
(count->phrase 1 "elephant")
(count->phrase 2 "elephant")
(count->phrase 0 "goose" "geese")
(count->phrase 1 "goose" "geese")
(count->phrase 2 "goose" "geese")
]
}

@section{error}
@require[(for-label mischief/error)]
@defmodule[mischief/error]
