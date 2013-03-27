#lang scribble/manual

@title{@racketmodname[debug]: Explicit Debugging}
@require[(for-label debug)]
@defmodule[debug]

@deftogether[(
@defform*[((define/debug id expr) (define/debug (head args) body ...+))]{}
@defform[(define-values/debug (id ...) expr)]{}
@defform[(lambda/debug kw-formals body ...+)]{}
@defform[(case-lambda/debug [formals body ...+] ...)]{}
@defform[(#%app/debug proc-expr arg ...)]{}
)]{
The @racketid[/debug] variant of each form behaves the same as the original
from @racketmodname[racket], and additionally produces debugging output.
}

@defform[(debug proc-expr arg ...)]{
An alias for @racket[#%app/debug].
}

@defform[(debug* . expr)]{
Behaves the same as @racket[expr], with additional debugging output before and
after its execution.
}

@defproc[(dprintf [fmt string?] [arg any/c] ...) void?]{
Reports @racket[(format fmt arg ...)] as debugging output.
}

@defproc[(stylish-dprintf [fmt string?] [arg any/c] ...) void?]{
Reports @racket[(stylish-format fmt arg ...)] as debugging output.
}

@defproc[(call-and-debug [fmt string?] [arg any/c] ...
           [#:thunk proc (-> any)])
         any]{
Invokes @racket[proc] and produces debugging output,
annotated with @racket[(stylish-format fmt arg ...)]
}

@defproc[(debug-value [fmt string?] [arg any/c] ... [#:value x any/c]) void?]{
Reports that the value @racket[x] is returned, annotated with
@racket[(stylish-format fmt arg ...)].
}

@defproc[(debug-values [fmt string?] [arg any/c] ... [#:value xs list?]) void?]{
Reports that the values in @racket[xs] are returned, annotated with
@racket[(stylish-format fmt arg ...)].
}

@defproc[(debug-exception [fmt string?] [arg any/c] ... [#:exn x any/c]) void?]{
Reports that the value @racket[x] is @racket[raise]d as an exception, annotated
with @racket[(stylish-format fmt arg...)].
}

@section{@racketmodname[debug/syntax]: Debugging for Macros}
@require[(for-label debug/syntax)]
@defmodule[debug/syntax]

@deftogether[(
@defform*[((define-syntax/debug id expr) (define-syntax/debug (head args) body ...+))]{}
@defform[(define-syntaxes/debug (id ...) expr)]{}
)]{
The @racketid[/debug] variants of @racket[define-syntax] and
@racket[define-syntaxes] produce debugging output at compile-time (phase 1
relative to their binding).
}

@section{@racketmodname[debug/racket]: Implicit Debugging for Racket}

@defmodulelang[debug/racket]

The @racketmodname[debug/racket] language provides the same bindings as
@racketmodname[racket] except that it uses the @racketid[/debug]-suffixed
definitions from @racketmodname[debug] in place of the originals.  The language
also provides @racket[define-syntax/debug] as @racketid[define-syntax] at phase
-1, so that any macro transformers defined using @racket[debug/racket] will
generate appropriate output.

@section{@racketmodname[debug/mischief]: Implicit Debugging for Mischief}

@defmodulelang[debug/mischief]

The @racketmodname[debug/mischief] language provides the same bindings as
@racketmodname[mischief] except that it uses the @racketid[/debug]-suffixed
definitions from @racketmodname[debug] in place of the originals.  The language
also provides @racket[define-syntax/debug] as @racketid[define-syntax] at phase
-1, so that any macro transformers defined using @racket[debug/mischief] will
generate appropriate output.
