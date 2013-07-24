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
@defproc[(symbol-foldcase [sym symbol?]) symbol?]
)]{

Convert the case of a symbol's name, by analogy with @racket[string-upcase],
@racket[string-downcase], @racket[string-titlecase], and
@racket[string-foldcase].

@data-examples[
(symbol-upcase 'Two-words)
(symbol-downcase 'Two-words)
(symbol-titlecase 'Two-words)
(symbol-foldcase 'Two-words)
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
@defproc[(keyword-foldcase [sym keyword?]) keyword?]
)]{

Convert the case of a keyword's name, by analogy with @racket[string-upcase],
@racket[string-downcase], @racket[string-titlecase], and
@racket[string-foldcase].

@data-examples[
(keyword-upcase '#:Two-words)
(keyword-downcase '#:Two-words)
(keyword-titlecase '#:Two-words)
(keyword-foldcase '#:Two-words)
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
(transparent-struct-type-name [x transparent-struct?]
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

@section[#:tag "dict"]{@racketmodname[mischief/dict]: Dictionaries}

@defmodule[mischief/dict]

@defproc[
(dict-ref? [dict dict?] [key any/c]
  [#:success success (-> any/c any) identity]
  [#:failure failure any/c
   (lambda {}
     (error 'dict-ref? "key ~v not found in dict ~v" key dict))])
any
]{

Looks up @racket[key] in @racket[dict].  If @racket[dict] has a mapping for
@racket[key], calls @racket[success] with the associated value.  Otherwise,
invokes @racket[failure] if it is a procedure, and returns it otherwise.

@data-examples[
(define (search table name)
  (dict-ref? table name
    #:failure (lambda {} (gensym name))
    #:success (lambda {id} (search table id))))
(define table (hash 'y 'x))
(search table 'x)
(search table 'y)
(search table 'z)
]

}

@defproc[
(dict-update? [dict dict?] [key any/c]
  [#:transform proc (-> any/c any/c)]
  [#:success success (-> any/c any/c) identity]
  [#:failure failure any/c
   (lambda {}
     (error 'dict-update? "key ~v not found in dict ~v" key dict))])
dict?
]{

Updates the value for @racket[key] in @racket[dict].  If @racket[key] is bound
to some value @racket[x] in @racket[dict], updates the binding to
@racket[(proc (success x))].  Otherwise, binds @racket[key] to
@racket[(proc (failure))] if @racket[failure] is a procedure, and
@racket[(proc failure)] otherwise.

@data-examples[
(define (insert key val table)
  (dict-update? table key
    #:transform (lambda {vals} (cons val vals))
    #:success (lambda {vals} (remove val vals))
    #:failure (lambda {} '())))
(define table
  (hash 'a '(x y z)))
(insert 'a 'z
  (insert 'b 'x
    table))
]

}

@defproc[
(dict-add [d0 dict?] [d dict?] ...
  [#:combine combine (or/c #false (-> any/c any/c any/c)) #false]
  [#:combine/key combine/key (or/c #false (-> any/c any/c any/c any/c))
   (if combine
     (lambda {k v1 v2} (combine v1 v2))
     #false)])
dict?
]{

Adds the bindings in each @racket[d] to @racket[d0].  When one key @racket[k]
has two values @racket[v1] and @racket[v2], binds @racket[k] to
@racket[(combine/key k v1 v2)] in the result.

@data-examples[
(dict-add (hasheq 'a 'b) (hash 'x 'y))
]

}

@defproc[
(dict-subtract [d0 dict?] [d dict?] ...)
dict?
]{

Removes the bindings for the keys in each @racket[d] from @racket[d0], if they
exist.

@data-examples[
(dict-subtract (hasheq 'a 1 'b 2 'c 3) (hash 'a "team" 'b "hive"))
]

}

@defproc[
(dict-set-all [d dict?] [#:value value any/c] [seq sequence?] ...)
dict?
]{

Binds every value @racket[x] from each sequence @racket[seq] in @racket[d] to
@racket[(value x)] if @racket[x] is a procedure, and to @racket[x] otherwise.

@data-examples[
(dict-set-all (hash) #:value symbol->string '(a b c))
]

}

@defproc[
(dict-remove-all [d dict?] [seq sequence?] ...)
dict?
]{

Removes every value @racket[x] in each sequence @racket[seq] from @racket[d].

@data-examples[
(dict-remove-all (hash 'a 1 'b 2 'c 3) '(a b))
]

}

@defproc[
(dict->procedure [dict dict?]
  [#:failure failure any/c
   (lambda {key}
     (error 'dict->procedure "key ~v not found in dict ~v" key dict))])
(-> any/c any)
]{

Returns a procedure that looks up its argument in @racket[dict] and returns the
associated value.  If a key @racket[k] is not found, calls @racket[(failure k)]
if it is a procedure and returns @racket[failure] otherwise.

@data-examples[
(define lookup (dict->procedure (hash 'a 1 'b 2 'c 3) #:failure 0))
(lookup 'a)
(lookup 'x)
]

}

@section[#:tag "stream"]{@racketmodname[mischief/stream]: Streams}

@defmodule[mischief/stream]

@defform[(define-stream id expr)]{

Defines @racket[id] as a lazy stream encapsulating @racket[expr].

@data-examples[
(define-stream naturals
  (stream-cons 0 (stream-map add1 naturals)))
naturals
(stream-ref naturals 0)
(stream-ref naturals 1)
(stream-ref naturals 2)
]

}

@defform[(stream-delay expr)]{

Constructs a lazy stream encapsulating @racket[expr].

@data-examples[
(define delayed-stream
  (stream-delay '(1 2 3)))
delayed-stream
(stream? delayed-stream)
(stream-empty? delayed-stream)
(stream-first delayed-stream)
(stream? (stream-delay 'not-a-stream))
(stream-empty? (stream-delay 'not-a-stream))
]

}

@defform[(stream* x ... st) #:contracts ([x any/c] [st stream?])]{

Creates a lazy stream that contains each @racket[x] followed by the contents of
@racket[st].

@data-examples[
(define (nonzero-integers-from k)
  (stream* k (- k) (nonzero-integers-from (add1 k))))
(define nonzero-integers
  (nonzero-integers-from 1))
(stream-ref nonzero-integers 0)
(stream-ref nonzero-integers 1)
(stream-ref nonzero-integers 2)
(stream-ref nonzero-integers 3)
(stream-ref nonzero-integers 4)
]

}

@defproc[(stream-take [st stream?] [n exact-nonnegative-integer?]) list?]{

Produces a list containing the first @racket[n] elements of @racket[st].

@data-examples[
(stream-take (stream 1 2 3 (error "Oops!")) 3)
]

@defproc[(stream-zip [st stream?] ...+) stream?]{

Creates a lazy stream containing lists of the respective elements of each
@racket[st].

@data-examples[
(stream->list
  (stream-zip (stream 1 2 3) (stream 'a 'b 'c)))
]

}

@defproc[(stream-interleave [st stream?] ...) stream?]{

Creates a lazy stream that alternates the elements of the given @racket[st]s.

@data-examples[
(stream->list
  (stream-interleave (stream 1 2 3) (stream 'a 'b 'c)))
]

}

@defproc[(stream-interleave* [st stream?]) stream?]{

Creates a lazy stream from @racket[st], which must be a stream of streams.  The
resulting stream includes all of the elements from the streams in @racket[st],
even if @racket[st] is an infinite stream of streams.

@data-examples[
(stream->list
  (stream-interleave*
    (stream (stream 1 2 3) (stream 'a 'b 'c))))
]

}

@defproc[(stream-cross-product
           [f (-> any/c any/c any/c)]
           [st1 stream?]
           [st2 stream?])
         stream?]{

Creates a lazy stream whose elements are the results of applying @racket[f] to
every pairwise combination of elements from @racket[st1] and @racket[st2].

@data-examples[
(stream->list
  (stream-cross-product
    string-ref
    (stream "cat" "dog")
    (stream 2 1 0)))
]

}

@defproc[(stream-cross-product*
           [f (-> any/c any/c any/c)]
           [st1 stream?]
           [st2 stream?])
         stream?]{

Creates a lazy stream of streams whose elements are the results of applying
@racket[f] to every pairwise combination of elements from @racket[st1] and
@racket[st2].

@data-examples[
(map stream->list
  (stream->list
    (stream-cross-product*
      string-ref
      (stream "cat" "dog")
      (stream 2 1 0))))
]

}

}

@section[#:tag "maybe"]{@racketmodname[mischief/maybe]: Optional Values}

@defmodule[mischief/maybe]

@defstruct*[yes ([value any/c]) #:transparent]{

A struct that can be used for optional values that can include @racket[#false].

}

@defproc[(no? [x any/c]) boolean?]{

Equivalent to @racket[(false? x)].

}

@defproc[(no) #false]{

Returns @racket[#false].

}

@defproc[(maybe? [x any/c]) boolean?]{

Equivalent to @racket[(or (yes? x) (no? x))].

}

@section[#:tag "quotation"]{@racketmodname[mischief/quotation]:
Quoting Values as S-expressions}

@defmodule[mischief/quotation]

@defproc[
(quotation [x any/c]
  [#:custom custom
   (-> any/c (or/c (-> (-> any/c any/c) any/c any/c) #false))
   (const #false)])
any/c
]{

Produces an s-expression representing an expression that would evaluate to
@racket[x].  Can be customized using @racketvalfont{#:custom}; see
@racket[custom-quoter] below for an example.

@data-examples[
(quotation (list (vector 1 2) (box "three")))
]

}

@defproc[
(custom-quoter
  [pred predicate/c]
  [proc (-> (-> any/c any/c) any/c any/c)])
(-> any/c (or/c (-> (-> any/c any/c) any/c any/c) #false))
]{

Produces a custom quotation procedure that quotes values satisfying
@racket[pred] using @racket[proc] and leaves everything else to default
quotation.

@data-examples[
(struct both [one two])
(define (quote-both rec b)
  `(both
     ,(rec (both-one b))
     ,(rec (both-two b))))
(quotation (both (list (both 1 2)) 3)
  #:custom (custom-quoter both? quote-both))
]

}

@section[#:tag "sort"]{@racketmodname[mischief/sort]:
Topological Sorting}

@defmodule[mischief/sort]

@defproc[
(topological-sort [nodes list?] [neighbors (-> any/c list?)]
  [#:cycle cycle (-> list? none/c)
   (lambda {xs}
     (error 'topological-sort "cycle detected: ~v" xs))])
list?
]{

Topologically sorts the values in @racket[nodes], interpreted as a graph where
each node's neighbors are determined by @racket[neighbors].  If a cycle is
encountered, an error is raised using @racket[cycle].

@data-examples[
(define nodes '(a b c d))
(define neighbors
  (dict->procedure #:failure (const empty)
    (hash 'a '(b c) 'b '(d) 'c '(d))))
(topological-sort nodes neighbors)
]

}
