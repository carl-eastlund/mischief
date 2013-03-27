#lang scribble/manual

@(require mischief/examples)

@(define-example-form transform-examples mischief)

@title[#:tag "transform"]{@racketmodname[mischief/transform]: Syntax Transformers}
@defmodule[mischief/transform]

@section{Converting Values to/from Syntax}

@defproc[
(to-syntax
  [x any/c]
  [#:stx stx (or/c syntax? #false) #false]
  [#:context context (or/c syntax? #false) stx]
  [#:source source source-location? stx]
  [#:prop prop (or/c syntax? #false) stx])
syntax?
]{
Converts @racket[x] to a syntax object using the lexical context of
@racket[context], the source location of @racket[source], and the syntax
properties of @racket[prop], using @racket[stx] for any of the above that are
not provided.  Equivalent to
@racket[(datum->syntax context x (build-source-location-list source) prop)].

@transform-examples[
(define stx (to-syntax 'car #:stx #'here))
stx
(build-source-location stx)
(free-identifier=? stx #'car)
]
}

@defproc[(to-datum [x any/c]) any/c]{
Replaces any syntax objects contained in @racket[x] with their content;
equivalent to @racket[(syntax->datum (datum->syntax #false x))].

@transform-examples[
(to-datum (list #'(quote (1 2 3)) #'(#%datum . "text")))
]
}

@defproc[(quote-transformer [x any/c]) syntax?]{
Produces a expression that reconstructs @racket[x] when evaluated.  Useful when
@racket[x] may contain syntax objects, which are not preserved by simply
wrapping @racket[x] in @racket[quote].

@transform-examples[
(to-datum (quote-transformer (list (quote (1 2)) (syntax (3 4)))))
]
}

@section{Constructing Identifiers}

@defproc[
(fresh [x (or/c symbol? string? identifier?) 'fresh]
  [#:source source source-location? (if (identifier? x) x #false)]
  [#:add-suffix? add-suffix? boolean? #true])
identifier?
]{
Constructs an identifier with a unique binding named @racket[x], with a unique
suffix if @racket[add-suffix?] is true, and using the source location of
@racket[source].

@transform-examples[
(fresh)
(fresh)
(define x #'x)
(define fresh-x (fresh x #:add-suffix? #false))
x
fresh-x
(free-identifier=? x fresh-x)
]
}

@defproc[
(format-fresh [fmt string?] [arg (or/c symbol? string? identifier?)] ...
  [#:source source source-location? #false]
  [#:add-suffix? add-suffix? boolean? #true])
identifier?
]{
Equivalent to:
@racketblock[
(fresh (format fmt arg ...)
  #:source source
  #:add-suffix? add-suffix?)
]

@transform-examples[
(format-fresh "~a-~a" #'string #'length)
]
}

@defproc[(fresh-mark) (-> syntax? syntax?)]{
An alias for @racket[make-syntax-introducer].

@transform-examples[
(define id1 (fresh))
(define mark (fresh-mark))
(define id2 (mark id1))
(define id3 (mark id2))
(bound-identifier=? id1 id2)
(bound-identifier=? id2 id3)
(bound-identifier=? id1 id3)
]
}

@deftogether[(
@defproc[(identifier-upcase [id identifier?]) identifier?]
@defproc[(identifier-downcase [id identifier?]) identifier?]
@defproc[(identifier-titlecase [id identifier?]) identifier?]
)]{
Change the letters in the name of an identifier by analogy to
@racket[string-upcase], @racket[string-downcase], and
@racket[string-titlecase].

@transform-examples[
(identifier-upcase #'Two-words)
(identifier-downcase #'Two-words)
(identifier-titlecase #'Two-words)
]
}
