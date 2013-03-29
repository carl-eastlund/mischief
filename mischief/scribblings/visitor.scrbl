#lang scribble/manual

@(require (for-label mischief))

@title[#:tag "visitor"]{@racketmodname[mischief/visitor]:
Compositional, Higher-Order Value Traversals}
@require[(for-label mischief/visitor)]
@defmodule[mischief/visitor]

@defproc[
(visit [visitor visitor?] [target any/c] [arg-or-keyword-arg any/c] ...)
any
]{

Invokes @racket[visitor] on @racket[target], passing on the given
@racket[arg-or-keyword-arg]s.

}

@defproc[(visitor? [x any/c]) boolean?]{
Recognizes visitors.
}

@defproc[(visitor-combine [visitor visitor?] ...) visitor?]{

Constructs a compound visitor that tries the given @racket[visitor]s from left
to right to find one that can process any given value.

}

@defproc[
(make-default-visitor
  [handler (-> any/c any/c ... any/c)])
visitor?
]{

Constructs a visitor that always calls @racket[handler] with the target value
and any additional arguments.

}

@defproc[
(make-leaf-visitor
  [predicate (-> any/c boolean?)]
  [handler (-> any/c any/c ... any/c)])
visitor?
]{

Constructs a visitor that handles target values that satisfy
@racket[predicate].  These target values are passed to @racket[handler] along
with any additional arguments that were passed to the visitor.

}

@defproc[
(make-visitor
  [predicate (-> any/c boolean?)]
  [handler (-> (-> any/c any/c ... any/c) any/c any/c ... any/c)])
visitor?
]{

Constructs a visitor that handles target values that satisfy
@racket[predicate].  The handler is called with a recursive handler procedure,
the target value, and any additional arguments that were passed to the visitor.

}

@defproc[
(make-wrapper-visitor
  [handler
   (-> (-> any/c any/c ... any/c)
       (-> any/c any/c ... any/c)
       any/c any/c ... any/c)])
visitor?
]{

Constructs a visitor that processes all target values, given two recursive
handler procedures.  The first resumes the search for a relevant visitor after
the current one within a compound visitor, and is usually used for the given
value when the current visitor is not relevant.  The second uses the whole
visitor in use, and is intended for recursive handling of sub-parts.

}

@deftogether[(
@defproc[
(make-uniform-default-visitor [handler (-> any/c any/c)])
visitor?
]
@defproc[
(make-uniform-leaf-visitor
  [predicate (-> any/c boolean?)]
  [handler (-> any/c any/c)])
visitor?
]
@defproc[
(make-uniform-visitor
  [predicate (-> any/c boolean?)]
  [handler (-> (-> any/c any/c) any/c)])
visitor?
]
@defproc[
(make-uniform-wrapper-visitor
  [handler (-> (-> any/c any/c) (-> any/c any/c) any/c any/c)])
visitor?
]
)]{

Variants of @racket[make-default-visitor], @racket[make-leaf-visitor],
@racket[make-visitor], and @racket[make-wrapper-visitor] that ignore any
additional arguments, and automatically pass them on unchange to all recursive
visitor calls.

}

@defthing[map-visitor visitor?]{

A visitor that reconstructs its target value.

}

@defthing[for-each-visitor visitor?]{

A visitor that executes side effects for all parts of its target value, and
always produces @racket[(void)].

}

@defthing[map/reduce-visitor visitor?]{

A visitor that expects two additional arguments: a unary @emph{map} function to
transform individual elements of the target value, and a variable-arity
@emph{reduce} function to combine results.

}

@defproc[
(make-map-visitor
  [predicate (-> any/c boolean?)]
  [constructor (-> any/c ... predicate)]
  [accessor (-> predicate any/c)] ...)
visitor?
]{

Creates a visitor that handles values satisfying @racket[predicate] by
extracting their fields with the given @racket[accessor]s, recursively visiting
each, then reassembling the result using the @racket[constructor].

}

@defproc[
(make-for-each-visitor
  [predicate (-> any/c boolean?)]
  [accessor (-> predicate/c any/c)] ...)
visitor?
]{

Creates a visitor that handles values satisfying @racket[predicate] by
extracting their fields with the given @racket[accessor]s and recursively
visiting each.  Always returns @racket[(void)].

}

@defproc[
(make-map/reduce-visitor
  [predicate (-> any/c boolean?)]
  [accessor (-> predicate/c any/c)] ...)
visitor?
]{

Creates a visitor that handles values satisfying @racket[predicate] by
extracting their fields with the given @racket[accessor]s, recursively visiting
each, then reassembling the result using the visitor's @emph{reduce} function.

}

@defproc[
(make-memoizing-visitor [memo memo-table? (make-memo-table)])
visitor?
]{

Creates a visitor that memoizes the results of passing each target value to
subsequent visitors.

}
