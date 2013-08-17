#lang scribble/manual

@(require scribble/eval mischief/examples)

@title[#:tag "shorthand"]{Writing Simple Macros with @racketmodname[mischief/shorthand]}
@require[(for-label mischief/shorthand)]
@defmodule[mischief/shorthand]

@(define-example-evaluator shorthand-eval mischief (for-syntax mischief))

@defform*[(
(define-shorthand (id . pattern) template)
(define-shorthand id [pattern template] ...)
)]{
Defines @racket[id] as a macro that matches applications against each
@racket[pattern] and produces the corresponding @racket[template] for the first
match.  Non-identifier macros with only a single pattern can use the simplified
form that combines the pattern with the macro name.  Each @racket[pattern] is
interpreted as in @racket[syntax-parse].

@examples[#:eval shorthand-eval
(define-shorthand (lam x e) (lambda {x} e))
(define-shorthand def
  [(_ x:id e) (define x e)]
  [(_ (f x) e) (def f (lam x e))])
(def (len seq) (for/fold {[n 0]} {[x seq]} (add1 n)))
(len (list 1 2 3))
(len (vector 1 2 3))
]
}

@defform[(define-id-shorthand name template)]{

Defines @racket[name] as an identifier macro that expands by replacing
@racket[name] with @racket[template].

@examples[#:eval shorthand-eval
(define counter (box 0))
(define-id-shorthand current (unbox counter))
(define-id-shorthand increment!
  (set-box! counter (add1 current)))
current
increment!
current
increment!
current
(current 'bad 'application)
]

}

@deftogether[(
@defform[(define-alias id target-id)]
@defform[(define-aliases [id target-id] ...)]
)]{
Defines each @racket[id] as an alias for the corresponding @racket[target-id]
using @racket[make-rename-transformer].

@examples[#:eval shorthand-eval
(define-alias value quote)
(define-aliases [choose cond] [otherwise else])
(define (sum xs)
  (choose
    [(empty? xs) (value 0)]
    [otherwise (+ (first xs) (sum (rest xs)))]))
(sum (list 1 2 3 4))
]
}

@defform[(with-aliases {[id target-id] ...} body ...+)]{
Defines each @racket[id] as a local alias for the corresponding
@racket[target-id] within the @racket[body] of the expression.

@examples[#:eval shorthand-eval
(with-aliases {[cake pi]} (* cake 2))
cake
]
}
