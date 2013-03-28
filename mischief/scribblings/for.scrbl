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

@deftogether[(
@defform[(for/fold/lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(for*/fold/lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(for/fold/filter-lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(for*/fold/filter-lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(for/fold/append-lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(for*/fold/append-lists {[x e] ...} {y ...} for-clauses . for-body)]
)]{

Loops that combine folded results with accumulated lists.

@for-examples[
(for/fold/lists
    {[len 0]}
    {syms}
    {[str (in-list '("one" "two" "three"))]}
  (values
    (+ len (string-length str))
    (string->symbol str)))
(define table (hash 'a "apple" 'b "banana"))
(for/fold/filter-lists
    {[longest 0]}
    {hits}
    {[key (in-list '(a b c))]
     #:when (dict-has-key? table key)}
  (define str (dict-ref table key))
  (values
    (max longest (string-length str))
    str))
(for/fold/append-lists
    {[count 0]}
    {chars}
    {[word (in-list '(cat a log))]}
  (values
    (add1 count)
    (string->list (symbol->string word))))
]

}

@deftogether[(
@defform[(define/for/fold {[x e] ...} for-clauses . for-body)]
@defform[(define/for*/fold {[x e] ...} for-clauses . for-body)]
@defform[(define/for/fold/lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(define/for*/fold/lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(define/for/lists {x ...} for-clauses . for-body)]
@defform[(define/for*/lists {x ...} for-clauses . for-body)]
@defform[(define/for/filter-lists {x ...} for-clauses . for-body)]
@defform[(define/for*/filter-lists {x ...} for-clauses . for-body)]
@defform[(define/for/append-lists {x ...} for-clauses . for-body)]
@defform[(define/for*/append-lists {x ...} for-clauses . for-body)]
@defform[(define/for/partition {x y} for-clauses . for-body)]
@defform[(define/for*/partition {x y} for-clauses . for-body)]
@defform[(define/for/partition-lists {[x y] ...} for-clauses . for-body)]
@defform[(define/for*/partition-lists {[x y] ...} for-clauses . for-body)]
@defform[(define/for/fold/filter-lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(define/for*/fold/filter-lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(define/for/fold/append-lists {[x e] ...} {y ...} for-clauses . for-body)]
@defform[(define/for*/fold/append-lists {[x e] ...} {y ...} for-clauses . for-body)]
)]{

Definitions based on loops.

@for-examples[
(define/for/fold {[vowels 0]} {[c (in-string "beautiful")]}
  (+ vowels (if (memv c (list #\a #\e #\i #\o #\u)) 1 0)))
vowels
]

}
