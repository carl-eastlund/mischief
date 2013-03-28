#lang mischief

(provide
  define-syntax/debug
  define-syntaxes/debug)

(require
  (for-syntax
    mischief
    debug))

(define-syntax (define-syntax/debug stx)
  (syntax-parse stx
    [(_ (name:id . args:kw-formals) . body:block-body)
     (define/syntax-parse [desc-fmt desc-arg ...]
       (syntax->format/args (attribute name)))
     #'(define-syntax (name . args)
         (!dbg call-and-debug desc-fmt desc-arg ...
           #:thunk
           (lambda ()
             (!dbg debug-value "Argument ~s:" 'args.formal-id
               #:value args.formal-id)
             ...
             . body)))]
    [(_ name:id body:expr)
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args  (attribute name)))
     #'(define-syntax name
         (!dbg call-and-debug fmt arg ...
           #:thunk (lambda () (#%expression body))))]))

(define-syntax (define-syntaxes/debug stx)
  (syntax-parse stx
    [(_ (~and names {_:id ...}) body:expr)
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args  (attribute names)))
     #'(define-syntaxes names
         (!dbg call-and-debug fmt arg ...
           #:thunk (lambda () (#%expression body))))]))

(begin-for-syntax

  (define (syntax->format/args stx)
    (define/syntax-parse e stx)
    (cond
      [(source-location-known? stx)
       (list #'"~s [~a]"
         #'(quote e)
         #'(source-location->string (quote-srcloc e)))]
      [else
       (list #'"~s"
         #'(quote e))]))

  ;; Who debugs the debugger:
  (define !dbg
    call #;
    (let* {[!depth 0]}
      (lambda/keywords (ks vs proc . xs)
        (define name (or (object-name proc) proc))
        (dynamic-wind
          (lambda ()
            (set! !depth (add1 !depth))
            (eprintf "!!>> ~a ~a\n" !depth
              (keyword-apply format-application ks vs proc xs)))
          (lambda ()
            (define results
              (call-with-values
                (lambda ()
                  (with-handlers
                      {[(lambda (x) #true)
                        (lambda (e)
                          (eprintf "!!-- ~a ~a\n" !depth (format-exception e))
                          (raise e))]}
                    (keyword-apply proc ks vs xs)))
                list))
            (eprintf "!!-- ~a ~a\n" !depth (format-values results))
            (apply values results))
          (lambda ()
            (eprintf "!!<< ~a ~s\n" !depth name)
            (set! !depth (sub1 !depth))))))))
