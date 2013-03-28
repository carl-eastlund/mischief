#lang scribble/manual

@(require mischief/examples)

@(define-example-form for-examples mischief)

@title[#:tag "for"]{@racketmodname[mischief/for]: For Loops}

@defmodule[mischief/for]

@deftogether[(
@defform[(for/hash! for-clauses . for-body)]
@defform[(for*/hash! for-clauses . for-body)]
@defform[(for/hasheq! for-clauses . for-body)]
@defform[(for*/hasheq! for-clauses . for-body)]
@defform[(for/hasheqv! for-clauses . for-body)]
@defform[(for*/hasheqv! for-clauses . for-body)]
)]{

Loops for creating mutable hash tables, by analogy with @racket[for/hash] and
similar forms.

@for-examples[
(define keys '(one two three four))
(define vals '(1 2 3 4))
(for/hash! {[k (in-list keys)] [v (in-list vals)]} (values k v))
(for/hasheq! {[k (in-list keys)] [v (in-list vals)]} (values k v))
(for/hasheqv! {[k (in-list keys)] [v (in-list vals)]} (values k v))
]

}

@deftogether[(
@defform[(for/dict init-expr for-clauses . for-body)]
@defform[(for*/dict init-expr for-clauses . for-body)]
@defform[(for/dict! init-expr for-clauses . for-body)]
@defform[(for*/dict! init-expr for-clauses . for-body)]
)]{

Loops for creating immutable and mutable dictionaries.  They work by analogy
with @racket[for/hash] and similar forms, with the addition of an explicit
@racket[init-expr] specifying a dictionary to add the bindings to.

@for-examples[
(for/dict '([a one] [b two])
    {[k (in-list '(b c))]
     [v (in-list '("B" "C"))]}
  (values k v))
(define vec (vector 0 0 0 0))
(for/dict! vec {[i (in-range 4)]}
  (values i (* i i)))
vec
]

}

@deftogether[(
@defform[(for/filter for-clauses . for-body)]
@defform[(for*/filter for-clauses . for-body)]
@defform[(for/filter-lists {list-id ...} for-clauses . for-body)]
@defform[(for*/filter-lists {list-id ...} for-clauses . for-body)]
)]{

Loops for creating lists and discarding @racket[#false] elements.

@for-examples[
(for*/filter {[vowel (in-list '(a e i o u))]
              [word (in-list '([c a t] [b i r d] [m o u s e]))]}
  (memq vowel word))
(define (root x)
  (define y (sqrt x))
  (and (exact-integer? y) y))
(define (half x)
  (define y (/ x 2))
  (and (exact-integer? y) y))
(for/filter-lists {roots halves} {[i (in-range 10)]}
  (values (root i) (half i)))
]

}

@deftogether[(
@defform[(for/append for-clauses . for-body)]
@defform[(for*/append for-clauses . for-body)]
@defform[(for/append-lists {list-id ...} for-clauses . for-body)]
@defform[(for*/append-lists {list-id ...} for-clauses . for-body)]
)]{

Loops for appending lists.

@for-examples[
(for/append {[str (in-list '("cat" "a" "log"))]}
  (string->list str))
(for/append-lists {evens odds} {[i (in-range 10)]}
  (partition even? (build-list i add1)))
]

}

@deftogether[(
@defform[(for/partition for-clauses . for-body)]
@defform[(for*/partition for-clauses . for-body)]
@defform[(for/partition-lists {[yes-id no-id] ...} for-clauses . for-body)]
@defform[(for*/partition-lists {[yes-id no-id] ...} for-clauses . for-body)]
)]{

Loops for partitioning results into two categories.

@for-examples[
(for/partition {[{i s} (in-dict (vector 'a 'e 'i 'o 'u))]}
  (values (even? i) s))
(for/partition-lists {[evens odds] [lefts rights]}
    {[{i s} (in-dict (vector 'a 'e 'i 'o 'u))]}
  (values (even? i) i s))
]

}
