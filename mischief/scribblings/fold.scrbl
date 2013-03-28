#lang scribble/manual

@(require (for-label mischief))

@title[#:tag "fold"]{@racketmodname[mischief/fold]: Folding Over Common Datatypes}
@require[(for-label mischief/fold)]
@defmodule[mischief/fold]

@defproc[

(datum-fold [x any/c]
  [#:short-circuit fold-short-circuit
   (-> any/c (-> any/c) any/c)
   default-fold-short-circuit]
  [#:list fold-list (-> list? any/c) default-fold-list]
  [#:list* fold-list* (-> list? (not/c list?) any/c) default-fold-list*]
  [#:vector fold-vector (-> list? any/c) default-fold-vector]
  [#:box fold-box (-> any/c any/c) default-fold-box]
  [#:prefab fold-prefab (-> prefab-key? list? any/c) default-fold-prefab]
  [#:hash fold-hash (-> hash? list? list? any/c) default-fold-hash]
  [#:hash-eq fold-hash-eq
   (-> list? list? any/c)
   (arg+ default-fold-hash (hasheq))]
  [#:hash-eqv fold-hash-eqv
   (-> list? list? any/c)
   (arg+ default-fold-hash (hasheqv))]
  [#:hash-equal fold-hash-equal
   (-> list? list? any/c)
   (arg+ default-fold-hash (hash))]
  [#:syntax fold-syntax (-> syntax? any/c any/c) default-fold-syntax]
  [#:other fold-other (-> (-> any/c any/c) any/c any/c) default-fold-other])

any/c

]{

Constructs a result by folding over @racket[x].  At each recursive step, first
calls @racket[fold-short-circuit] with the current input value and a thunk that
computes the result for that value.  If that thunk is called, continues using
@racket[fold-list], @racket[fold-list*], @racket[fold-vector],
@racket[fold-box], @racket[fold-prefab], @racket[fold-hash-eq],
@racket[fold-hash-eqv], @racket[fold-hash-equal], @racket[fold-syntax], and
@racket[fold-other] as appropriate.  All of the above functions except
@racket[fold-other] are called with the result of recurring on their sub-parts;
@racket[fold-other] is called with a recursive folding procedure and the
current input value.

}

@deftogether[(
@defproc[(default-fold-short-circuit [x any/c] [proc (-> any/c)]) any/c]
@defproc[(default-fold-list [xs list?]) any/c]
@defproc[(default-fold-list* [head list?] [tail (not/c list?)]) any/c]
@defproc[(default-fold-vector [xs list?]) any/c]
@defproc[(default-fold-box [x any/c]) any/c]
@defproc[(default-fold-prefab [key prefab-key?] [fields list?]) any/c]
@defproc[(default-fold-hash [table hash?] [keys list?] [vals list?]) any/c]
@defproc[(default-fold-syntax [stx syntax?] [x any/c]) any/c]
@defproc[(default-fold-other [proc (-> any/c any/c)] [x any/c]) any/c]
)]{

Default folding functions.  The @racket[default-fold-short-circuit] function
simply calls the provided thunk; @racket[default-fold-other] returns its second
argument.  The other functions reconstitute their arguments into the
corresponding data type (list, improper list, etc.).

}
