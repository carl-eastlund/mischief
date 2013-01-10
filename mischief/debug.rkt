#lang racket/base

(provide
  define/debug
  lambda/debug
  case-lambda/debug
  #%app/debug
  debug
  debug*
  debug-value
  debug-values
  dprintf
  stylish-dprintf
  call-with-debug-frame
  call-and-debug)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    syntax/srcloc
    mischief/racket/boolean
    mischief/syntax/srcloc
    mischief/syntax/parse)
  racket/match
  racket/function
  syntax/srcloc
  mischief/racket/boolean
  mischief/racket/error
  mischief/racket/string
  mischief/racket/function
  mischief/racket/stylish
  mischief/syntax/location)

(define-syntax (debug stx)

  (define-splicing-syntax-class actual
    (pattern (~seq value:expr)
      #:attr [key-prefix 1] '())
    (pattern (~seq key:keyword value:expr)
      #:attr [key-prefix 1] (list (attribute key))))

  (syntax-parse stx
    [(_ . (~and app (fun:expr arg:actual ...)))
     (define/syntax-parse fun-var (generate-temporary (attribute fun)))
     (define/syntax-parse [arg-var ...]
       (map generate-temporary (attribute arg.value)))
     (define/syntax-parse {[arg-key/var ...] ...}
       #'{[arg.key-prefix ... arg-var] ...})
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args "application: " (attribute app)))
     #'(!dbg call-and-debug fmt arg ...
         #:thunk
         (lambda ()
           (let {[fun-var (debug* . fun)]
                 [arg-var (debug* . arg.value)]
                 ...}
             (fun-var arg-key/var ... ...))))]))

(define-syntax #%app/debug
  (make-rename-transformer #'debug))

(begin-for-syntax
  (define (syntax->format/args prefix stx)
    (define/syntax-parse e stx)
    (cond
      [(source-location-known? stx)
       (list #'"~a~s [~a]"
         #`(quote #,prefix)
         #'(quote e)
         #'(source-location->string (quote-srcloc/smart e)))]
      [else
       (list #'"~a~s"
         #`(quote #,prefix)
         #'(quote e))])))

(define-syntax (debug* stx)
  (syntax-parse stx
    [(_ . e:expr)
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args "expression: " (attribute e)))
     #'(!dbg call-and-debug fmt arg ...
         #:thunk (lambda () (#%expression e)))]))

(define-syntax (define/debug stx)
  (syntax-parse stx
    [(_ (name:id . args:kw-formals) . body:block-body)
     (define/syntax-parse [desc-fmt desc-arg ...]
       (syntax->format/args "function: " (attribute name)))
     #'(define (name . args)
         (!dbg call-and-debug desc-fmt desc-arg ...
           #:thunk
           (lambda ()
             (!dbg debug-value "Argument ~s:" 'args.formal-id
               #:value args.formal-id)
             ...
             . body)))]
    [(_ name:id body:expr)
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args "definition: " (attribute name)))
     #'(define name
         (!dbg call-and-debug fmt arg ...
           #:thunk (lambda () (#%expression body))))]))

(define-syntax (lambda/debug stx)
  (syntax-parse stx
    [(form args:kw-formals . body:block-body)
     (define/syntax-parse [desc-fmt desc-arg ...]
       (syntax->format/args "" (attribute form)))
     #'(lambda args
         (!dbg call-and-debug desc-fmt desc-arg ...
           #:thunk
           (lambda ()
             (!dbg debug-value "Argument ~s:" 'args.formal-id
               #:value args.formal-id)
             ...
             . body)))]))

(define-syntax (case-lambda/debug stx)
  (syntax-parse stx
    [(form [args:formals . body:block-body] ...)
     (define/syntax-parse [desc-fmt desc-arg ...]
       (syntax->format/args "" (attribute form)))
     #'(case-lambda
         [args
          (!dbg call-and-debug desc-fmt desc-arg ...
            #:thunk
            (lambda ()
              (!dbg debug-value "Argument ~s:" 'args.formal-id
                #:value args.formal-id)
              ...
              . body))]
         ...)]))

(define (call-and-debug #:thunk thunk fmt . args)
  (apply !dbg call-with-debug-frame fmt args
    #:thunk
    (lambda ()
      (call-with-values
        (lambda ()
          (with-handlers
              {[exn:fail?
                (lambda (x)
                  (!dbg debug-exception "Exception:" #:exn x)
                  (raise x))]}
            (thunk)))
        (lambda results
          (!dbg debug-values "Result:" #:values results)
          (apply values results))))))

(define (debug-value #:value x fmt . args)
  (apply !dbg debug-values #:values (list x) fmt args))

(define (debug-values #:values xs fmt . args)
  (apply !dbg debug-expr #:expr (values->expr xs) fmt args))

(define (debug-exception #:exn x fmt . args)
  (apply !dbg debug-expr #:expr (exn->expr x) fmt args))

(define (debug-expr #:expr e fmt . args)
  (!dbg stylish-dprintf "~f ~s" (list* fmt args) e))

(define (values->expr xs)
  (match xs
    [(list x) (stylish-value->expr x)]
    [_ (list* 'values (map stylish-value->expr xs))]))

(define (exn->expr x)
  (cond!
    [(and (exn? x) (not (struct? x)))
     (list 'error (exn-message x))]
    [else
     (list 'raise (stylish-value->expr x))]))

(define (call-with-debug-frame
          #:enter [enter-prefix (current-debug-enter-prefix)]
          #:exit [exit-prefix (current-debug-exit-prefix)]
          #:thunk thunk
          fmt . args)
  (define (enter) (apply !dbg stylish-dprintf #:prefix enter-prefix fmt args))
  (define (exit) (apply !dbg stylish-dprintf #:prefix exit-prefix fmt args))
  (define (work)
    (parameterize {[current-debug-depth (add1 (current-debug-depth))]}
      (thunk)))
  (dynamic-wind enter work exit))

(define (dprintf
          #:prefix [prefix (current-debug-prefix)]
          fmt . args)
  (eprintf "~a"
    (indent prefix
      (apply format fmt args))))

(define (stylish-dprintf
          #:prefix [prefix (current-debug-prefix)]
          #:columns [columns (current-stylish-print-columns)]
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          fmt . args)
  (!dbg dprintf #:prefix prefix "~a"
    (apply !dbg stylish-format
      #:columns (max 1 (- columns (indent-length) (string-length prefix)))
      #:expr-style est
      #:print-style pst
      fmt args)))

(define (indent prefix str)
  (define n (indent-length))
  (define indentation (make-string n #\space))
  (apply string-append
    (for/list {[line (in-list (string-lines str))]}
      (format "~a~a~a\n" indentation prefix line))))

(define (indent-length)
  (* (debug-indent-width) (current-debug-depth)))

(define (debug-indent-width) 1)

(define current-debug-depth (make-parameter 0))
(define current-debug-enter-prefix (make-parameter ">> "))
(define current-debug-exit-prefix (make-parameter "<< "))
(define current-debug-prefix (make-parameter "| "))

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
          (set! !depth (sub1 !depth)))))))
