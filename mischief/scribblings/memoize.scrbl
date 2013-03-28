#lang scribble/manual

@(require (for-label mischief))

@title[#:tag "memoize"]{@racketmodname[mischief/memoize]: Memoized Functions}
@require[(for-label mischief/memoize)]
@defmodule[mischief/memoize]

@deftogether[(
@defform[(define/memoize (name . kw-formals) body ...+)]
@defform[(lambda/memoize kw-formals body ...+)]
@defform[(case-lambda/memoize [formals body ...+] ...)]
@defform[(let/memoize loop-id {[x e] ...} body ...+)]
)]{

Variants of @racket[define] (for functions), @racket[lambda],
@racket[case-lambda], and (named) @racket[let] that produce memoized
functions.

}

@defproc[
(memoize-procedure [proc procedure?])
procedure?
]{

Produces a memoized version of @racket[proc].

}

@defproc[
(call/memoize
  [memo memo-table?]
  [proc procedure?]
  [arg-or-keyword-arg any/c] ...)
any
]{

If the sequence of positional and keyword arguments has an entry in
@racket[memo], produces the stored result.  Otherwise, passes the arguments to
@racket[proc] and records the result in @racket[memo] before returning (or
raising) it.

}

@defproc[
(memoize [memo memo-table?] [key any/c] ... [#:compute proc (-> any)])
any
]{

If the sequence of @racket[key]s has an entry in @racket[memo], produces the
stored result.  Otherwise, calls @racket[proc] and records the result in
@racket[memo] before returning (or raising) it.

}

@defproc[(memo-table? [x any/c]) boolean?]{

Recognizes memoization tables.

}

@defproc[(make-memo-table) memo-table?]{

Creates a fresh memoization table.

}

@defproc[(clear-memo-table! [memo memo-table?]) void?]{

Erases the contents of @racket[memo].

}
