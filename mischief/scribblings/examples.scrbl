#lang scribble/manual

@(require (for-label mischief scribble/eval racket/sandbox))

@title[#:style '(toc)]{@racketmodname[mischief/examples]: Evaluating Examples
for Scribble Documentation}
@require[(for-label mischief/examples)]
@defmodule[mischief/examples]

@defform/subs[#:literals [for-syntax]
(examples/evaluator lang-module-path maybe-requires body ...+)
{[maybe-requires
  code:blank
  (code:line #:requires [require-mod ...])]
 [require-mod
  module-path
  (for-syntax module-path)]}
]{

Like @racket[examples] using an evaluator built using @racket[lang-module-path]
and @racket[maybe-requires].  Also requires the @racket[lang-module-path] and
@racket[maybe-requires] at the label phase.

}

@defform[(define-example-form name-id lang-module-path require-mod ...)]{

Defines a macro named @racket[name-id] that works like @racket[examples], but
using an evaluator based on @racket[lang-module-path] and the given
@racket[require-mod]s.  Also requires the @racket[lang-module-path] and
@racket[require-mod]s at the label phase.

}

@defform[(define-example-evaluator name-id lang-module-path require-mod ...)]{

Defines an evaluator named @racket[name-id] using
@racket[make-example-evaluator] and requires the @racket[lang-module-path] and
@racket[require-mod]s at the label phase.

}

@defproc[
(make-example-evaluator
  [language
   (or/c
     module-path?
     (list/c 'special symbol?)
     (cons/c 'begin list?))]
  [input-program any/c] ...
  [#:requires requires 
   (listof (or/c module-path? path-string? 
             (cons/c 'for-syntax (listof module-path?))))
   null]
  [#:allow-for-require allow-for-require
   (listof (or/c module-path? path?))
   null]
  [#:allow-for-load allow-for-load (listof path-string?) null]
  [#:allow-read allow-read (listof (or/c module-path? path-string?)) null])
(-> any/c any)
]{

Like @racket[make-evaluator], but called with permission and output parameters
set appropriately for generating Scribble documentation.

}
