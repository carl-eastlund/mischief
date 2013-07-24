#lang scribble/manual

@(require (for-label mischief))

@title[#:tag "id-table"]{@racketmodname[mischief/id-table]: Dictionaries for Identifiers}

@defmodule[mischief/id-table]

@defproc[(label-id-representative [id identifier?]) identifier?]{

Strips the bindings from @racket[id] and produces an identifier with the same
symbolic name and the same marks.

}

@defproc[(label-identifier=? [one identifier?] [two identifier?]) boolean?]{

Reports whether @racket[one] and @racket[two] have the same symbolic name and
marks, regardless of their bindings.  Equivalent to:
@racketblock[
(bound-identifier=?
  (label-id-representative one)
  (label-id-representative two))
]

}

@defproc[
(check-duplicate-label [ids (listof identifier?)])
(or/c identifier? #false)
]{

If any elements of @racket[ids] are @racket[label-identifier=?] to each other,
returns one of the duplicates.  Otherwise, returns @racket[#false].

}

@deftogether[(

@defproc[
(make-label-id-table
  [init (dict/c identifier? any/c) '()])
(dict/c identifier? any/c)
]

@defproc[
(make-immutable-label-id-table
  [init (dict/c identifier? any/c) '()])
(dict/c identifier? any/c)
]

@defproc[
(make-weak-label-id-table
  [init (dict/c identifier? any/c) '()])
(dict/c identifier? any/c)
]

)]{

Creates a hash table-based dictionary---mutable, immutable, or weak,
respectively---that maps identifiers based on
@racket[label-identifier-hash-code] and @racket[label-identifier=?].  Populates
the hash table with the contents of @racket[init].

}

@defproc[
(label-identifier-hash-code
  [id identifier?]
  [rec (-> any/c exact-integer?) equal-hash-code])
exact-integer?]{

Computes a unique hash code for @racket[id] that is consistent with
@racket[label-identifier=?], using @racket[rec] to compute the hash code for
@racket[id]'s symbolic name.

}

@defproc[
(free-identifier-hash-code
  [id identifier?]
  [rec (-> any/c exact-integer?) equal-hash-code]
  [#:phase phase
   (or/c exact-nonnegative-integer? #false) 
   (syntax-local-phase-level)])
exact-integer?]{

Computes a unique hash code for @racket[id] that is consistent with
@racket[free-identifier=?] in @racket[phase], using @racket[rec] to compute the
hash code for the symbolic name of @racket[id]'s binding.

}

@defproc[
(bound-identifier-hash-code
  [id identifier?]
  [rec (-> any/c exact-integer?) equal-hash-code]
  [#:phase phase
   (or/c exact-nonnegative-integer? #false) 
   (syntax-local-phase-level)])
exact-integer?]{

Computes a unique hash code for @racket[id] that is consistent with
@racket[bound-identifier=?] in @racket[phase], using @racket[rec] to compute
the hash code for @racket[id]'s symbolic name.

}
