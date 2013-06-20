#lang racket/base

(provide
  module-expansion-sexp
  syntax-expansion-sexp
  (struct-out step)
  (struct-out misstep)
  (struct-out remarkstep)
  (struct-out state)
  (struct-out bigframe))

(require
  racket/function
  racket/promise
  syntax/modcode
  macro-debugger/model/trace
  macro-debugger/model/reductions
  macro-debugger/model/reductions-config
  (prefix-in original-
    macro-debugger/model/steps))

(struct step [type s1 s2] #:prefab)
(struct misstep [type s1 exn] #:prefab)
(struct remarkstep [type s1 contents] #:prefab)
(struct state [e foci ctx lctx binders uses frontier seq] #:prefab)
(struct bigframe [ctx foci e] #:prefab)

(define (module-expansion-sexp mod)
  (syntax-expansion-sexp (module-syntax mod)))

(define (module-syntax mod)
  (get-module-code mod
    #:compile identity
    #:choose (lambda paths 'src)))

(define (syntax-expansion-sexp stx)
  (steps->sexp (syntax-expansion-steps stx)))

(define (syntax-expansion-steps stx)
  (parameterize {[macro-policy (lambda (id-stx) #t)]}
    (reductions (trace stx))))

(define (steps->sexp ss)
  (for/vector {[step (in-list ss)]}
    (step->sexp step)))

(define (step->sexp s)
  (define type (original-protostep-type s))
  (define s1 (state->sexp (original-protostep-s1 s)))
  (cond
    [(original-step? s)
     (step type s1 (state->sexp (original-step-s2 s)))]
    [(original-misstep? s)
     (misstep type s1 (exn->sexp (original-misstep-exn s)))]
    [(original-remarkstep? s)
     (remarkstep type s1 (original-remarkstep-contents s))]))

(define (exn->sexp e) e)

(define (state->sexp st)
  (state
    (original-state-e st)
    (original-state-foci st)
    (context->sexp (original-state-ctx st))
    (big-context->sexp (original-state-lctx st))
    (original-state-binders st)
    (original-state-uses st)
    (original-state-frontier st)
    (original-state-seq st)))

(define (context->sexp fs)
  (delay (original-context-fill fs here)))

(define (big-context->sexp bfs)
  (map big-frame->sexp bfs))

(define (big-frame->sexp bf)
  (bigframe
    (context->sexp (original-bigframe-ctx bf))
    (original-bigframe-foci bf)
    (original-bigframe-e bf)))

(define here
  (datum->syntax #f '|[ HOLE ]|))
