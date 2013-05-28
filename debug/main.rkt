#lang racket/base

(provide
  define/debug
  define-values/debug
  lambda/debug
  case-lambda/debug
  #%app/debug
  debug
  debug*
  debug-value
  debug-values
  debug-exception
  dprintf
  stylish-dprintf
  call-and-debug
  call-with-debug-frame)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    syntax/srcloc
    mischief/boolean
    mischief/parse)
  racket/match
  racket/function
  syntax/srcloc
  syntax/location
  mischief/match
  mischief/boolean
  mischief/error
  mischief/string
  mischief/function
  mischief/stylish
  no-debug/low-level)

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
     #'(low-level-debug call-and-debug fmt arg ...
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
         #'(source-location->string (quote-srcloc e)))]
      [else
       (list #'"~a~s"
         #`(quote #,prefix)
         #'(quote e))])))

(define-syntax (debug* stx)
  (syntax-parse stx
    [(_ . e:expr)
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args "expression: " (attribute e)))
     #'(low-level-debug call-and-debug fmt arg ...
         #:thunk (lambda () (#%expression e)))]))

(define-syntax (define/debug stx)
  (syntax-parse stx
    [(_ (name:id . args:kw-formals) . body:block-body)
     (define/syntax-parse [desc-fmt desc-arg ...]
       (syntax->format/args "function: " (attribute name)))
     #'(define (name . args)
         (low-level-debug call-and-debug desc-fmt desc-arg ...
           #:thunk
           (lambda ()
             (low-level-debug debug-value "Argument ~s:" 'args.formal-id
               #:value args.formal-id)
             ...
             . body)))]
    [(_ name:id body:expr)
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args "definition: " (attribute name)))
     #'(define name
         (low-level-debug call-and-debug fmt arg ...
           #:thunk (lambda () (#%expression body))))]))

(define-syntax (define-values/debug stx)
  (syntax-parse stx
    [(_ (~and names {_:id ...}) body:expr)
     (define/syntax-parse [fmt arg ...]
       (syntax->format/args "definition: " (attribute names)))
     #'(define-values names
         (low-level-debug call-and-debug fmt arg ...
           #:thunk (lambda () (#%expression body))))]))

(define-syntax (lambda/debug stx)
  (syntax-parse stx
    [(form args:kw-formals . body:block-body)
     (define/syntax-parse [desc-fmt desc-arg ...]
       (syntax->format/args "" (attribute form)))
     #'(lambda args
         (low-level-debug call-and-debug desc-fmt desc-arg ...
           #:thunk
           (lambda ()
             (low-level-debug debug-value "Argument ~s:" 'args.formal-id
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
          (low-level-debug call-and-debug desc-fmt desc-arg ...
            #:thunk
            (lambda ()
              (low-level-debug debug-value "Argument ~s:" 'args.formal-id
                #:value args.formal-id)
              ...
              . body))]
         ...)]))

(define (call-and-debug #:thunk thunk fmt . args)
  (apply low-level-debug call-with-debug-frame fmt args
    #:thunk
    (lambda ()
      (call-with-values
        (lambda ()
          (with-handlers
              {[exn:fail?
                (lambda (x)
                  (low-level-debug debug-exception "Exception:" #:exn x)
                  (raise x))]}
            (thunk)))
        (lambda results
          (low-level-debug debug-values "Result:" #:values results)
          (apply values results))))))

(define (debug-value #:value x fmt . args)
  (apply low-level-debug debug-values #:values (list x) fmt args))

(define (debug-values #:values xs fmt . args)
  (apply low-level-debug debug-expr #:expr (values->expr xs) fmt args))

(define (debug-exception #:exn x fmt . args)
  (apply low-level-debug debug-expr #:expr (exn->expr x) fmt args))

(define (debug-expr #:expr e fmt . args)
  (low-level-debug stylish-dprintf "~f ~s" (list* fmt args) e))

(define (values->expr xs)
  (match! xs
    [(list x) (low-level-debug stylish-value->expr x)]
    [_ (list* 'values (low-level-debug map stylish-value->expr xs))]))

(define (exn->expr x)
  (cond!
    [(and (exn? x) (not (struct? x)))
     (list 'error (exn-message x))]
    [else
     (list 'raise (low-level-debug stylish-value->expr x))]))

(define (call-with-debug-frame
          #:enter [enter-prefix (debug-prefix 'enter)]
          #:exit [exit-prefix (debug-prefix 'exit)]
          #:thunk thunk
          fmt . args)
  (define (enter)
    (apply low-level-debug stylish-dprintf #:prefix enter-prefix fmt args))
  (define (exit)
    (apply low-level-debug stylish-dprintf #:prefix exit-prefix fmt args))
  (define (work)
    (parameterize {[current-debug-depth (add1 (current-debug-depth))]}
      (low-level-debug thunk)))
  (low-level-debug dynamic-wind enter work exit))

(define (dprintf
          #:prefix [prefix (debug-prefix)]
          fmt . args)
  (low-level-debug eprintf "~a"
    (low-level-debug prefix-lines prefix
      (low-level-debug apply format fmt args))))

(define (stylish-dprintf
          #:prefix [prefix (debug-prefix)]
          #:columns [columns (current-stylish-print-columns)]
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          fmt . args)
  (low-level-debug dprintf #:prefix prefix "~a"
    (apply low-level-debug stylish-format
      #:columns (max 1 (- columns (string-length prefix)))
      #:expr-style est
      #:print-style pst
      fmt args)))

(define (prefix-lines prefix str)
  (apply string-append
    (for/list {[line (in-list (string-lines str))]}
      (string-append prefix line "\n"))))

(define (debug-prefix [event #false])
  ((current-debug-indent) (current-debug-depth) event))

(define (default-debug-indent depth event)
  (format "#;~a~a "
    (make-string depth #\space)
    (match! event
      ['enter ">>"]
      ['exit "<<"]
      [#false "!"])))

(define current-debug-depth (make-parameter 0))
(define current-debug-indent (make-parameter default-debug-indent))
