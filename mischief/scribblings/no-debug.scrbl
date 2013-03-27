#lang scribble/manual

@title{@racketmodname[no-debug]: Disabled Debugging}
@require[(for-label no-debug)]
@defmodule[no-debug]

@deftogether[(
@defform*[((define/debug id expr) (define/debug (head args) body ...+))]{}
@defform[(define-values/debug (id ...) expr)]{}
@defform[(lambda/debug kw-formals body ...+)]{}
@defform[(case-lambda/debug [formals body ...+] ...)]{}
@defform[(#%app/debug proc-expr arg ...)]{}
)]{
These forms are aliases for the same forms without the @racketid[/debug] suffix
from @racketmodname[racket].
}

@defform[(debug proc-expr arg ...)]{
An alias for @racket[#%app].
}

@defform[(debug* . expr)]{
Equivalent to @racket[expr].
}

@defproc[(dprintf [fmt string?] [arg any/c] ...) void?]{
Equivalent to @racket[(void)].
}

@defproc[(stylish-dprintf [fmt string?] [arg any/c] ...) void?]{
Equivalent to @racket[(void)].
}

@defproc[(call-and-debug [fmt string?] [arg any/c] ...
           [#:thunk proc (-> any)])
         any]{
Equivalent to @racket[(proc)].
}

@defproc[(debug-value [fmt string?] [arg any/c] ... [#:value x any/c]) void?]{
Equivalent to @racket[(void)].
}

@defproc[(debug-values [fmt string?] [arg any/c] ... [#:value xs list?]) void?]{
Equivalent to @racket[(void)].
}

@defproc[(debug-exception [fmt string?] [arg any/c] ... [#:exn x any/c]) void?]{
Equivalent to @racket[(void)].
}

@section{@racketmodname[no-debug/syntax]: Disabled Debugging for Macros}
@require[(for-label no-debug/syntax)]
@defmodule[no-debug/syntax]

@deftogether[(
@defform*[((define-syntax/debug id expr) (define-syntax/debug (head args) body ...+))]{}
@defform[(define-syntaxes/debug (id ...) expr)]{}
)]{
Aliases for @racket[define-syntax] and @racket[define-syntaxes].
}

@section{@racketmodname[debug/racket]: Disabled Debugging for Racket}

@defmodulelang[no-debug/racket]

The @racketmodname[no-debug/racket] language is equivalent to
@racketmodname[racket].

@section{@racketmodname[debug/mischief]: Disabled Debugging for Mischief}

@defmodulelang[no-debug/mischief]

The @racketmodname[no-debug/mischief] language is equivalent to
@racketmodname[mischief].
