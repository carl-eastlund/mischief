#lang scribble/manual

@(require (for-label mischief))

@title{Modules}

@section{@racketmodname[mischief/require]: Importing from Modules}
@require[(for-label mischief/require)]
@defmodule[mischief/require]

@defform[(dynamic-in mod-id)]{

Equivalent to @racket[(quote mod-id)] when used in @racket[require].

}

@defform[(require/provide require-spec ...)]{

Requires the @racket[require-spec]s and provides all the resulting bindings.
When each @racket[require-spec] is a module path, equivalent to:
@racketblock[
(begin
  (require require-spec ...)
  (provide (all-from-out require-spec ...)))
]

}

@defform[(quote-require require-spec ...)]{

Produces a list of symbols representing the names imported by the
@racket[require-spec]s.

}

@section{@racketmodname[mischief/module]: Module Paths and Names}
@require[(for-label mischief/module)]
@defmodule[mischief/module]

@defproc[
(resolve-module-path [mod-path module-path?]
  [#:namespace namespace namespace? (current-namespace)]
  [#:module-name-resolver resolver
   (-> module-path? 
       (or/c resolved-module-path? #false)
       (or/c syntax? #false)
       boolean?
       resolved-module-path?)
   (current-module-name-resolver)]
  [#:relative-to relative-to (or/c resolved-module-path? #false) #false]
  [#:syntax-context syntax-context (or/c syntax? #false) #false]
  [#:load? load? boolean? #true])
resolved-module-path?
]{

Resolves @racket[mod-path].  Equivalent to:
@racketblock[
(parameterize {[current-namespace namespace]}
  (resolver mod-path relative-to syntax-context load?))
]

}

@defproc[(module-name? [x any/c]) boolean?]{

Recognizes values that are valid results for
@racket[resolved-module-path-name].

}
