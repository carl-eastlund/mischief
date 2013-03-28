#lang scribble/manual

@(require mischief/examples)

@title[#:style '(toc)]{First-Order Datatypes}

@(define-example-form data-examples mischief)

@section{list}
@require[(for-label mischief/list)]
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

@section{string}
@require[(for-label mischief/string)]
@defmodule[mischief/string]

@section{symbol}
@require[(for-label mischief/symbol)]
@defmodule[mischief/symbol]

@section{keyword}
@require[(for-label mischief/keyword)]
@defmodule[mischief/keyword]

@section{boolean}
@require[(for-label mischief/boolean)]
@defmodule[mischief/boolean]

@section{struct}
@require[(for-label mischief/struct)]
@defmodule[mischief/struct]

@section{dict}
@require[(for-label mischief/dict)]
@defmodule[mischief/dict]

@section{maybe}
@require[(for-label mischief/maybe)]
@defmodule[mischief/maybe]

@section{quotation}
@require[(for-label mischief/quotation)]
@defmodule[mischief/quotation]

@section{sort}
@require[(for-label mischief/sort)]
@defmodule[mischief/sort]
