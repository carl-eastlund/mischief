#lang scribble/manual

@(require mischief/examples)

@title[#:style '(toc)]{First-Order Datatypes}

@(define-example-form data-examples mischief)

@section[#:tag "list"]{@racketmodname[mischief/list]: Lists and Pairs}

@defmodule[mischief/list]

@defproc[(make-alist [keys list?] [value any/c]) (listof cons?)]{

Returns an association list in which the @racket[car]s of the elements are the
given @racket[keys] in order and the @racket[cdr] of each element is
@racket[value].

@data-examples[
(make-alist '(1 2 3) 'number)
]

}

@defproc[(build-alist [keys list?] [proc (-> any/c any/c)]) (listof cons?)]{

Returns an association list in which the @racket[car]s of the elements are the
given @racket[keys] in order and the @racket[cdr] of each element is
the result of @racket[proc] applied to its @racket[car].

@data-examples[
(build-alist '(one two three) symbol->string)
]

}

@defproc[
(map-map [proc (-> any/c ...+ any/c)] [xs (listof list?)] ...+)
(listof list?)
]{
Maps @racket[proc] over each element in one or more lists of lists.

@data-examples[
(map-map string->symbol '(("bat" "cat") ("hat" "sat") ("mat")))
(map-map + '((1 2) (3)) '((4 5) (6)))
]

}

@deftogether[(
@defproc[
(member? [x any/c] [xs list?] [equiv? (-> any/c any/c boolean?) equal?])
boolean?
]
@defproc[(memv? [x any/c] [xs list?]) boolean?]
@defproc[(memq? [x any/c] [xs list?]) boolean?]
)]{

Variants of @racket[member], @racket[memv], and @racket[memq] that return
@racket[#true] instead of a suffix of @racket[xs].

@data-examples[
(member? "cat" '("bat" "cat" "hat"))
(member? "mat" '("bat" "cat" "hat"))
(memv? 3 '(1 2 3))
(memv? 12 '(1 2 3))
(memq? 'a '(a b c))
(memq? 'd '(a b c))
]

}

@defproc[
(partition* [pred (-> any/c ... boolean?)] [xs list?] ...)
(values list? ... list? ...)
]{

Partitions multiple lists based on @racket[pred] as a generalization of
@racket[partition].  Returns results as multiple values: the lists of
respective values for which @racket[pred] was true followed by the lists of
respective values for which @racket[pred] was false.

@data-examples[
(partition* = '[1 2 3 4] '[1.0 -2.0 3.0 -4.0])
]

}

@deftogether[(
@defproc[(take-while [pred predicate/c] [xs list?]) list?]
@defproc[(take-until [pred predicate/c] [xs list?]) list?]
@defproc[(drop-while [pred predicate/c] [xs list?]) list?]
@defproc[(drop-until [pred predicate/c] [xs list?]) list?]
)]{

Produce a prefix or suffix of @racket[xs] based on applying @racket[pred] to
elements at the front of @racket[xs].

@data-examples[
(take-while negative? '[-2 -1 0 1 2])
(take-until positive? '[-2 -1 0 1 2])
(drop-while negative? '[-2 -1 0 1 2])
(drop-until positive? '[-2 -1 0 1 2])
]

}

@defproc[
(sort/unique [xs list?] [<? (-> any/c any/c boolean?)]
  [#:key key (-> any/c any/c) identity]
  [#:cache-keys? cache-keys? boolean? #false])
list?
]{

As @racket[sort], but discards duplicate elements.

@data-examples[
(sort/unique (list 2 7 1 8 2 8) <)
]

}

@section[#:tag "string"]{@racketmodname[mischief/string]: Strings}

@defmodule[mischief/string]

@defproc[(string-lines [str string?]) (listof string?)]{

Splits @racket[str] into lines.

@data-examples[
(string-lines "The cat\nin the hat\ncame back.\n")
]

}

@section[#:tag "symbol"]{@racketmodname[mischief/symbol]: Symbols}

@defmodule[mischief/symbol]

This module re-exports @racket[symbol=?] from @racketmodname[racket/bool] and
@racket[format-symbol] from @racket[racket/syntax].

@deftogether[(
@defproc[(symbol<? [x symbol?] [y symbol?]) boolean?]
@defproc[(symbol>? [x symbol?] [y symbol?]) boolean?]
@defproc[(symbol<=? [x symbol?] [y symbol?]) boolean?]
@defproc[(symbol>=? [x symbol?] [y symbol?]) boolean?]
)]{

Compare two symbols lexicographically, essentially composing
@racket[symbol->string] with @racket[string<?], @racket[string>?],
@racket[string<=?], and @racket[string>=?].

@data-examples[
(symbol<? 'bat 'cat)
(symbol<? 'cat 'cat)
(symbol<? 'cat 'bat)
(symbol>? 'bat 'cat)
(symbol>? 'cat 'cat)
(symbol>? 'cat 'bat)
(symbol<=? 'bat 'cat)
(symbol<=? 'cat 'cat)
(symbol<=? 'cat 'bat)
(symbol>=? 'bat 'cat)
(symbol>=? 'cat 'cat)
(symbol>=? 'cat 'bat)
]

}

@deftogether[(
@defproc[(symbol-upcase [sym symbol?]) symbol?]
@defproc[(symbol-downcase [sym symbol?]) symbol?]
@defproc[(symbol-titlecase [sym symbol?]) symbol?]
)]{

Convert the case of a symbol's name, by analogy with @racket[string-upcase],
@racket[string-downcase], and @racket[string-titlecase].

@data-examples[
(symbol-upcase 'Two-words)
(symbol-downcase 'Two-words)
(symbol-titlecase 'Two-words)
]

}

@section[#:tag "keyword"]{@racketmodname[mischief/keyword]: Keywords}

@defmodule[mischief/keyword]

This module re-exports @racket[keyword<?] from @racketmodname[racket/base].

@deftogether[(
@defproc[(keyword=? [x keyword?] [y keyword?]) boolean?]
@defproc[(keyword>? [x keyword?] [y keyword?]) boolean?]
@defproc[(keyword<=? [x keyword?] [y keyword?]) boolean?]
@defproc[(keyword>=? [x keyword?] [y keyword?]) boolean?]
)]{

Compare two keywords lexicographically, essentially composing
@racket[keyword->string] with @racket[string=?], @racket[string>?],
@racket[string<=?], and @racket[string>=?].

@data-examples[
(keyword=? '#:bat '#:cat)
(keyword=? '#:cat '#:cat)
(keyword=? '#:cat '#:bat)
(keyword>? '#:bat '#:cat)
(keyword>? '#:cat '#:cat)
(keyword>? '#:cat '#:bat)
(keyword<=? '#:bat '#:cat)
(keyword<=? '#:cat '#:cat)
(keyword<=? '#:cat '#:bat)
(keyword>=? '#:bat '#:cat)
(keyword>=? '#:cat '#:cat)
(keyword>=? '#:cat '#:bat)
]

}

@deftogether[(
@defproc[(keyword-upcase [sym keyword?]) keyword?]
@defproc[(keyword-downcase [sym keyword?]) keyword?]
@defproc[(keyword-titlecase [sym keyword?]) keyword?]
)]{

Convert the case of a keyword's name, by analogy with @racket[string-upcase],
@racket[string-downcase], and @racket[string-titlecase].

@data-examples[
(keyword-upcase '#:Two-words)
(keyword-downcase '#:Two-words)
(keyword-titlecase '#:Two-words)
]

}

@deftogether[(
@defproc[(keyword->symbol [key keyword?]) symbol?]
@defproc[(symbol->keyword [sym symbol?]) keyword?]
)]{

Convert between symbols and keywords.

@data-examples[
(keyword->symbol '#:something)
(symbol->keyword 'anything)
]

}

@defproc[
(format-keyword
  [fmt string?]
  [arg (or/c symbol? string? identifier? keyword? char? number?)] ...)
keyword?
]{

Produces a keyword whose name is @racket[(format fmt arg ...)].  Like
@racket[format-symbol], the format string must use only @litchar{~a}
placeholders, and identifiers in the @racket[arg]s are automatically converted
to symbols.

@data-examples[
(format-keyword "is-~a?" #'list)
]

}

@section[#:tag "boolean"]{@racketmodname[mischief/boolean]: Booleans and Conditionals}

@defmodule[mischief/boolean]

This module re-exports @racket[implies] and @racket[xor] from
@racketmodname[racket/bool].

@defform[(cond! cond-clause ...)]{

A variant of @racket[cond!] that raises an exception if all clauses fail.

@data-examples[
(define (sum xs)
  (cond!
    [(empty? xs) 0]
    [(cons? xs) (+ (first xs) (sum (rest xs)))]))
(sum (list 1 2 3))
(sum (vector 1 2 3))
]

}

@defstruct*[(exn:fail:cond! exn:fail) () #:transparent]{

The exception type raised by @racket[cond!] when all clauses fail.

}

@deftogether[(
@defproc[(and? [x any/c] ...) any/c]
@defproc[(or? [x any/c] ...) any/c]
@defproc[(implies? [x any/c] [y any/c]) any/c]
)]{

Procedure versions of @racket[and], @racket[or], and @racket[implies].  Since
they are procedures rather than macros, they do not short circuit but they can
be passed as values to higher-order functions.

@data-examples[
(and? #true 1)
(and? #false 1)
(and? #false (error "no short-circuiting"))
(or? #false #false)
(or? 1 #false)
(or? 1 (error "no short-circuiting"))
(implies? 1 2)
(implies? #false 2)
(implies? #false (error "no short-circuiting"))
]

}

@defproc[(iff [x any/c] [y any/c]) boolean?]{

Returns @racket[#true] if @racket[x] and @racket[y] are either both true or
both false, and @racket[#false] otherwise.
Equivalent to @racket[(not (xor a b))].

@data-examples[
(iff 1 2)
(iff 1 #false)
(iff #false 2)
(iff #false #false)
]

}

@defproc[(cons/optional [x any/c] [y any/c]) any/c]{

Returns @racket[y] if @racket[x] is @racket[#false], and @racket[(cons x y)]
otherwise.

@data-examples[
(cons/optional 1 '(2 3))
(cons/optional #false '(2 3))
]

}

@defproc[(list/optional [x any/c] ...) (listof (not/c #false))]{

Produces a list of each @racket[x] that is not @racket[#false].

@data-examples[
(list/optional 1 #false 3 4 #false 6)
]

}

@defproc[(list*/optional [x any/c] ... [y any/c]) any/c]{

Adds each @racket[x] that is not @racket[#false] to the (possibly improper)
list @racket[y].

@data-examples[
(list*/optional 1 #false 2 #false '(5 6))
]

}

@section[#:tag "struct"]{@racketmodname[mischief/struct]: User-Defined Datatypes}

@defmodule[mischief/struct]

@subsection{Updating Structure Fields}

@defform[
(update struct-id val-expr [field-id val-expr] ...)
]{

Constructs a version of @racket[val-expr], which must be an instance of
@racket[struct-id], where the value of each field named @racket[field-id] is
replaced by the result of the corresponding @racket[val-expr].  Each
@racket[field-id] is bound to the field's original value when executing any of
the @racket[val-expr]s.

@data-examples[
(struct both [one two] #:transparent)
(define (swap b)
  (update both b [one two] [two one]))
(swap (both 1 2))
]

}

@subsection{Prefab Structures}

@defproc[(prefab [k prefab-key?] [v any/c] ...) prefab?]{

Constructs a prefab structure with key @racket[k] and one field for each
@racket[v].

@data-examples[
(prefab 'posn 1 2)
]

}

@defproc[(prefab? [x any/c]) boolean?]{

Reports whether @racket[x] is a prefab structure.

@data-examples[
(prefab? (prefab 'posn 1 2))
(prefab? '(posn 1 2))
]

}

@defproc[(prefab-of-key? [key prefab-key?] [x any/c]) boolean?]{

Reports whether @racket[x] is a prefab structure whose key is equivalent to
@racket[k].

@data-examples[
(prefab-of-key? 'posn (prefab 'posn 1 2))
(prefab-of-key? 'posn (prefab 'point 1 2))
(prefab-of-key? 'posn '(posn 1 2))
]

}

@defproc[(prefab-key [x prefab?]) prefab-key?]{

Produces the key of a prefab struct.  The result is always normalized in the
sense of @racket[normalize-prefab-key].

@data-examples[
(prefab-key (prefab 'posn 1 2))
]

}

@defproc[(prefab-fields [x prefab?]) list?]{

Produces a list of the fields of a prefab struct.

@data-examples[
(prefab-fields (prefab 'posn 1 2))
]

}

@defproc[(prefab-ref [x prefab?] [i exact-nonnegative-integer?]) any/c]{

Extracts the @racket[i]th field of @racket[x], indexed from 0.

@data-examples[
(prefab-ref (prefab 'posn 1 2) 0)
(prefab-ref (prefab 'posn 1 2) 1)
]

}

@defproc[(prefab-type-name [x prefab?]) symbol?]{

Produces the symbol representing the name of a prefab struct's key.

@data-examples[
(prefab-type-name (prefab 'posn 1 2))
]

}

@defproc[(normalize-prefab-key [key prefab-key?]) prefab-key?]{

Normalizes @racket[key].  For any two prefab struct keys @racket[k1] and
@racket[k2], the keys represent the same prefab struct type if and only if
@racket[(equal? (normalize-prefab-key k1) (normalize-prefab-key k2))].

@data-examples[
(normalize-prefab-key 'posn)
(normalize-prefab-key '(posn))
]

}

@subsection{Transparent Structures}

@defproc[
(transparent-struct? [x any/c]
  [#:inspector inspector inspector? (current-inspector)])
boolean?
]{

Reports whether @racket[x] is a transparent structure, i.e., @racket[inspector]
can access its structure type and all of its fields.

}

@defproc[
(transparent-struct-type [x transparent-struct?]
  [#:inspector inspector inspector? (current-inspector)])
struct-type?
]{

Produces the structure type of @racket[x].

}

@defproc[
(transparent-struct-predicate [x transparent-struct?]
  [#:inspector inspector inspector? (current-inspector)])
predicate/c
]{

Produces a predicate that recognizes values belonging to the same structure
type as @racket[x].

}

@defproc[
(transparent-struct-constructor [x transparent-struct?]
  [#:inspector inspector inspector? (current-inspector)])
(-> any/c ... transparent-struct?)
]{

Produces a procedure that constructs transparent structs of the same type as
@racket[x].

}

@defproc[
(transparent-struct-ref [x transparent-struct?] [i exact-nonnegative-integer?]
  [#:inspector inspector inspector? (current-inspector)])
any/c
]{

Produces the value of the @racket[i]th field of @racket[x], indexed from 0.

}

@defproc[
(transparent-struct-name [x transparent-struct?]
  [#:inspector inspector inspector? (current-inspector)])
symbol?
]{

Produces the name of @racket[x]'s structure type.

}

@defproc[
(transparent-struct-fields [x transparent-struct?]
  [#:inspector inspector inspector? (current-inspector)])
list?
]{

Produces a list of the values of @racket[x]'s fields.

}

@section{dict}

@defmodule[mischief/dict]

@section{maybe}

@defmodule[mischief/maybe]

@section{quotation}

@defmodule[mischief/quotation]

@section{sort}

@defmodule[mischief/sort]
