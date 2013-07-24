#lang scribble/manual

@(require mischief/examples)

@(define-example-form transform-examples mischief (for-syntax mischief))

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
(fresh [x (or/c symbol? string? identifier? keyword? char? number?) 'fresh]
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
(format-fresh [fmt string?]
  [arg (or/c symbol? string? identifier? keyword? char? number?)] ...
  [#:source source source-location? #false]
  [#:add-suffix? add-suffix? boolean? #true])
identifier?
]{
Equivalent to:
@racketblock[
(fresh (format-symbol fmt arg ...)
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
@defproc[(identifier-foldcase [id identifier?]) identifier?]
)]{

Change the letters in the name of an identifier by analogy to
@racket[string-upcase], @racket[string-downcase], @racket[string-titlecase],
and @racket[string-foldcase].

@transform-examples[
(identifier-upcase #'Two-words)
(identifier-downcase #'Two-words)
(identifier-titlecase #'Two-words)
(identifier-foldcase #'Two-words)
]
}

@section{Common Syntax Transformer Patterns}

@defproc[
(id-transform
  [original syntax?]
  [replace (or/c syntax? (-> identifier? syntax?))])
syntax?
]{
Transforms the identifier that controls the expansion of @racket[original] as a
macro application, @racket[set!] transformer application, or identifier macro
reference.  Replaces that identifier with @racket[replace] if @racket[replace]
is a syntax object, or with the result of applying @racket[replace] to the
identifier if @racket[replace] is a procedure.

@transform-examples[
(id-transform #'simple-identifier #'replacement)
(id-transform #'(simple macro application) #'replacement)
(id-transform #'(set! macro application) #'replacement)
]
}

@defproc[
(id-transformer [proc (-> identifier? syntax?)])
(and/c set!-transformer? (-> syntax? syntax?))
]{
Produces a @racket[set!] transformer that, when defined as a macro, applies
@racket[proc] to the name of the macro in any application.

@transform-examples[
(define x 1)
(define-syntax X (id-transformer identifier-downcase))
(set! X (add1 X))
X
]
}

@defproc[
(set!-transformer [proc (-> syntax? syntax?)])
(and/c set!-transformer? (-> syntax? syntax?))
]{
Produces a @racket[set!] transformer that can also be used as a procedure.

@transform-examples[
(define-for-syntax f
  (set!-transformer
    (lambda {x}
      #'(error 'undefined "Oh, no!"))))
(define-syntax (x stx) (f stx))
x
(x 1)
(set! x 1)
(define-syntax y f)
y
(y 1)
(set! y 1)
]
}

@deftogether[(
@defproc[
(rename-transformer [id identifier?])
(and/c (-> syntax? syntax?) rename-transformer?)
]
@defproc[
(rename-transformers [id identifier?] ...)
(values (and/c (-> syntax? syntax?) rename-transformer?) ...)
]
)]{
Produces one or many rename transformers that can also be used as procedures.

@transform-examples[
(define-syntaxes {pair left right}
  (rename-transformers #'cons #'car #'cdr))
(pair (left '(1 2)) (right '(3 4)))
(define-syntax (tuple stx)
  ((rename-transformer #'list) stx))
tuple
(tuple 1 2 3)
]
}

@section{Phase 1 Helpers}

@defproc[(syntax-error [stx syntax?] [fmt string?] [x any/c] ...) none/c]{

An alias for @racket[wrong-syntax].

}

@defproc[(syntax-local-variable-reference) variable-reference?]{
Produces an anonymous variable reference representing the context currently
being expanded.

@transform-examples[
(module var-ref-example racket
  (require (for-syntax mischief/transform))
  (define-syntax (macro stx)
    (printf "Transforming: ~s\n"
      (resolved-module-path-name
        (variable-reference->resolved-module-path
          (syntax-local-variable-reference))))
    #'(begin))
  (macro))
(require 'var-ref-example)
]
}

@defproc[
(check-missing-identifier
  [actual (listof identifier?)]
  [expected (listof identifier?)])
(or/c identifier? #false)
]{
If @racket[actual] is missing a @racket[free-identifier=?] counterpart for any
identifier in @racket[expected], produces the first such identifier found.
Otherwise, produces @racket[#false]

@transform-examples[
(check-missing-identifier
  (list #'cons #'car #'cdr)
  (list #'cons #'first #'rest))
]
}
