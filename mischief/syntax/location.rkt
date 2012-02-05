#lang racket/base

(provide
  quote-srcloc/smart)

(require
  (for-syntax
    racket/base
    syntax/parse
    syntax/srcloc
    setup/path-to-relative)
  racket/block
  syntax/srcloc
  setup/path-to-relative)

(define-syntax (quote-srcloc/smart stx)
  (syntax-parse stx
    [(_) #`(quote-srcloc/smart #,stx)]
    [(_ loc)
     (define/syntax-parse [src0 line col pos span]
       (build-source-location-list #'loc))
     (define rel
       (and (path-string? (attribute src0))
         (path->relative-string/library (attribute src0) #false)))
     (cond
       [(not (path-string? (attribute src0)))
        #'(make-srcloc 'src0 'line 'col 'pos 'span)]
       [(path->relative-string/library (attribute src0) #false)
        =>
        (lambda (src)
          (define/syntax-parse src rel)
          #'(make-srcloc 'src 'line 'col 'pos 'span))]
       [else
        #'(cond
            [(source-location-source (quote-syntax loc))
             =>
             (lambda (src0)
               (define src
                 (or (path->relative-string/library src0 #false) src0))
               (make-srcloc src 'line 'col 'pos 'span))]
            [else (make-srcloc #false #false #false #false #false)])])]))
