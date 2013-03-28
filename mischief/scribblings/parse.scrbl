#lang scribble/manual

@(require mischief/examples)

@(define-example-form parse-examples mischief (for-syntax mischief))

@title[#:tag "parse"]{@racketmodname[mischief/parse]: Tools for
@racket[syntax-parse]}
@defmodule[mischief/parse]

@section{Shorthand Macros}

@defform[(|@| attribute-id)]{
An alias for @racket[attribute].
}

@defform[(syntax-matches? expr pat ...)]{
Returns @racket[#true] if @racket[expr] produces syntax matched by any of the
patterns @racket[pat], or @racket[#false] otherwise.
}

@defform[(syntax-matcher pat ...)]{
Produces a predicate that recognizes syntax matched by any of the patterns
@racket[pat].
}

@defform[(syntax-class/c class-id)]{
Produces a contract that accepts syntax belonging to the syntax class named by
@racket[class-id].
}

@defform[(syntax-parse/c pat ...)]{
Produces a contract that accepts syntax matched by any of the patterns
@racket[pat].
}

@section{Syntax Classes}

@subsection{Formal Arguments}

@defidform[formals]{

A syntax class that parses formal arguments as used by @racket[#%plain-lambda]
and @racket[case-lambda].  The class has 6 attributes: @racketid[arg-id],
@racketid[rest], @racketid[rest-id?], @racketid[rest-id], @racketid[formal-id],
and @racketid[call].

The attribute @racketid[arg-id] has a depth of 1 and contains all the
positional, non-rest formal argument identifiers.

The attribute @racketid[rest] has a depth of 0 and contains the tail of the
(possibly improper) list of arguments: either a rest argument or @racket[()].

The attribute @racketid[rest-id?] has a depth of 0 and contains the rest
argument identifier if one is present, or @racket[#false] otherwise.

The attribute @racketid[rest-id] has a depth of 1.  If there is a rest argument,
the attribute contains just that identifier.  Otherwise the attribute is empty.

The attribute @racketid[formal-id] has a depth of 1 and contains the result of
appending @racketid[arg-id] with @racketid[rest-id].

The attribute @racketid[call] has a depth of 0.  It is bound to @racket[#%app]
if @racketid[rest-id] is empty and @racket[apply] otherwise.

@parse-examples[
(define (recursive stx)
  (syntax-parse stx
    [({~literal define} (name:id . args:formals) . _)
     #'(args.call name args.formal-id ...)]))
(recursive #'(define (print x port) ---etc---))
(recursive #'(define (printf fmt . args) ---etc---))
]
}

@defidform[kw-formals]{

A syntax class that parses formal arguments including keyword arguments and
optional arguments as used by @racket[lambda] and @racket[define].  The class
has 13 attributes: @racketid[req-id], @racketid[opt-id], @racketid[opt-expr],
@racketid[req-kw], @racketid[req-kw-id], @racketid[opt-kw],
@racketid[opt-kw-id], @racketid[opt-kw-expr], @racketid[rest],
@racketid[rest-id?], @racketid[rest-id], @racketid[formal-id], and
@racketid[call].

@parse-examples[
(syntax-parse
  '{a b [c "one"] [d "two"] #:w e #:x f #:y [g "three"] #:z [h "four"] . i}
  [args:kw-formals
   (stylish-println
     (list
       (|@| args.req-id)
       (|@| args.opt-id)
       (|@| args.opt-expr)
       (|@| args.req-kw)
       (|@| args.req-kw-id)
       (|@| args.opt-kw)
       (|@| args.opt-kw-id)
       (|@| args.opt-kw-expr)
       (|@| args.rest)
       (|@| args.rest-id?)
       (|@| args.rest-id)
       (|@| args.formal-id)
       (|@| args.call)))])
]

}

@subsection{For Loops}

@defidform[for-clauses]{

A syntax class that parses a sequence of clauses for a @racket[for]-like macro.
Has no attributes.

@parse-examples[
(define (f x)
  (syntax-parse x
    [c:for-clauses #'c]))
(f #'{[(k v) (in-dict (hash))] #:when (eq? k v)})
(f #'{[(k v) (in-dict (hash))] [k (in-naturals)]})
(f #'{[(k v) (in-dict (hash))] #:else "something"})
]

See @racket[for-body] for a more practical example.
}

@defidform[for-body]{

A syntax class that parses the body of a @racket[for]-like macro.  Has two
attributes: @racketid[head] and @racketid[tail]. The attribute @racketid[head]
has a depth of 1 and contains the interleaved definitions, expressions, and
break clauses that form most of the body.  The attribute @racketid[tail] has a
depth of 0 and contains the final expression of the body.

@parse-examples[
(define-syntax (for/string-set! stx)
  (syntax-parse stx
    [(_ target:expr clauses:for-clauses . body:for-body)
     #'(let {[s target]}
         (for clauses
           body.head ...
           (define-values {i c} body.tail)
           (string-set! s i c)))]))
(define s (string-copy "fox"))
(for/string-set! s
    {[i (in-naturals)]
     [c (in-string "abcdefghijklmnopqrstuvwxyz")]}
  #:break (>= i (string-length s))
  (values i c))
s
]
}

@subsection{Definition and Expression Bodies}

@defidform[block-body]{

A syntax class that parses the body of a form like @racket[lambda] or
@racket[let] that accepts definitions an expressions.  Equivalent to the
pattern @racket[(|_:expr| ...+)].

}

@subsection{Literal Data}

@defidform[self-quoting]{
A syntax class that recognizes self-quoting values.  Has no attributes.
}

@defidform[module-path]{

A syntax class that recognizes values whose content, produced via
@racket[syntax->datum], satisfies @racket[module-path?].  The attribute
@racketid[value] stores the content of the parsed syntax.

}

@defproc[(literal [x any/c]) syntax-class]{

A parametric syntax class that recognizes syntax whose content, produced via
@racket[syntax->datum], is @racket[equal?] to @racket[x].  The attribute
@racketid[value] stores the content of the parsed syntax.

}

@defproc[(datum-literal [pred predicate/c] [desc string?]) syntax-class]{

A parametric syntax class that recognizes syntax whose content, produced via
@racket[syntax->datum], satisfies @racket[pred].  The attribute
@racketid[value] stores the content of the parsed syntax.

}

@subsection{Identifier Bindings}

@defidform[temp-id]{

A syntax class that matches anything, and binds the attribute @racketid[temp]
to a fresh identifier.

}

@defidform[bound-id]{

A syntax class that matches identifiers with a module or lexical binding; i.e.,
@racket[identifier-binding] does not return @racket[#false].

}

@defidform[static-id]{

A syntax class that matches identifiers bound as syntax; i.e.,
@racket[syntax-local-value] does not fail.

}

@defproc[(static-binding [pred predicate/c] [desc string?]) syntax-class]{

A syntax class that matches identifiers bound as syntax to values that satisfy
the result of @racket[pred].  Uses @racket[scope-static-value] in place of
@racket[syntax-local-value], so this syntax class is sensitive to
@racket[current-scope].  However, it should still work in any context where
@racket[syntax-local-value] would.  Has two attributes: @racketid[value] and
@racketid[delta].

The attribute @racketid[value] contains the value that the matched identifier
is bound to.

The attribute @racketid[delta] contains a delta syntax introducer based on the
parsed identifier.  This value is a function that operates on syntax, applying
any marks that are present on the parsed identifier that were not present on
the original identifier that defined it.

@parse-examples[
(define-syntax (write-static-string stx)
  (syntax-parse stx
    [(_ (~var s (static-binding string? "a static string variable")))
     (displayln (|@| s.value))
     #'(begin)]))
(define-syntax x "x")
(write-static-string x)
(define-syntax y 'y)
(write-static-string y)
]

}

@defidform[struct-binding]{

A syntax class that parses identifiers bound to static information about a
structure, i.e., values satisfying @racket[struct-info?].  Has 8 attributes:
@racketid[value], @racketid[descriptor-id], @racketid[constructor-id],
@racketid[predicate-id], @racketid[accessor-id], @racketid[mutator-id],
@racketid[super-id], and @racketid[known-fields?].

The attribute @racketid[value] contains the @racket[struct-info?] value that
the parsed identifier is bound to.

The attributes @racketid[descriptor-id], @racketid[constructor-id],
@racketid[predicate-id], @racketid[accessor-id], @racketid[mutator-id],
and @racketid[super-id] correspond to the fields of the list produced by
@racket[extract-struct-info].  The attributes @racketid[accessor-id] and
@racketid[mutator-id] have depth 1, and unlike in @racket[extract-struct-info]
they are proper lists and are not reversed.

The attribute @racketid[known-fields?] contains a boolean that is
@racket[#true] if the attributes @racketid[accessor-id] and
@racketid[mutator-id] represent every field of the structure, and
@racket[#false] if there may be missing fields.

}

@defidform[struct-binding/known]{

A syntax class that parses identifiers bound to static information about a
structure, like @racket[struct-binding]; it further requires that all the
information about the structure must be known---none of the attributes may be
@racket[#false].

}

@defproc[
(struct-binding/check
  [#:all         all                boolean? #false]
  [#:descriptor  known-descriptor?  boolean? all]
  [#:constructor known-constructor? boolean? all]
  [#:predicate   known-predicate?   boolean? all]
  [#:fields      known-fields?      boolean? all]
  [#:super       known-super?       boolean? all]
  [#:mutable     known-mutators?    boolean? all])
syntax-class
]{

A syntax class that parses identifiers bound to static information about a
structure, like @racket[struct-binding]; it further requires that some or all
of the information about the structure must be known.

}

@section{Literal Sets}

@defform[
(define-literals/ids base-id opt ... [literal-id ...])
]{

Simultaneously defines a literal set named @racket[base-id]@racketid[-literals]
and a list of identifiers named @racket[base-id]@racketid[-ids] based on the
given @racket[literal-id]s.  Passes the given @racket[opt]ions on to
@racket[define-literal-set].

}

@defform[(require/define-literals/ids base-id require-spec)]{

Requires @racket[require-spec] and defines both a literal set named
@racket[base-id]@racketid[-literals] and a list of identifiers named
@racket[base-id]@racketid[-ids] based on the imported bindings.

}
