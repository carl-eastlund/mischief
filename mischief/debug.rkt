#lang racket/base

(provide
  define/debug
  debug
  debug*
  debug-expr
  debug-value
  dprintf
  stylish-dprintf
  call-with-debug-frame
  call-and-debug)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    mischief/syntax/srcloc
    mischief/syntax/parse)
  racket/match
  mischief/racket/string
  mischief/racket/stylish)

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
     (define/syntax-parse desc
       (format "~s~a"
         (datum->syntax #false (attribute app))
         (source-location->suffix (attribute app))))
     #'(call-and-debug 'desc
         (lambda ()
           (let {[fun-var (debug-expr fun)]
                 [arg-var (debug-expr arg.value)]
                 ...}
             (fun-var arg-key/var ... ...))))]))

(define-syntax (debug* stx)
  (syntax-parse stx
    [(_ . e:expr)
     #'(debug-expr e)]))

(define-syntax (debug-expr stx)
  (syntax-parse stx
    [(_ e:expr)
     (define/syntax-parse desc
       (format "~s~a"
         (datum->syntax #false (attribute e))
         (source-location->suffix (attribute e))))
     #'(call-and-debug 'desc
         (lambda () (#%expression e)))]))

(define-syntax (define/debug stx)
  (syntax-parse stx
    [(_ (name:id . args:kw-formals) . body:block-body)
     (define/syntax-parse desc
       (format "~s~a"
         (syntax-e (attribute name))
         (source-location->suffix stx)))
     (define/syntax-parse [arg-desc ...]
       (for/list {[arg-id (in-list (attribute args.formal-id))]}
         (format "Argument ~s:" (syntax-e arg-id))))
     #'(define (name . args)
         (call-and-debug 'desc
           (lambda ()
             (debug-value 'arg-desc args.formal-id) ...
             . body)))]
    [(_ name:id body:expr)
     (define/syntax-parse desc
       (format "~s~a"
         (syntax-e (attribute name))
         (source-location->suffix stx)))
     #'(define name
         (call-and-debug 'desc
           (lambda () (#%expression body))))]))

(define (call-and-debug desc-str thunk)
  (call-with-debug-frame desc-str
    (lambda ()
      (call-with-values
        (lambda ()
          (with-handlers
              {[exn:fail?
                (lambda (x)
                  (debug-value "Raised exception:" x)
                  (raise x))]}
            (thunk)))
        (lambda results
          (debug-values "Result:" results)
          (apply values results))))))

(define (debug-value desc x)
  (debug-values desc (list x)))

(define (debug-values desc xs)
  (define indentation 2)
  (dprintf "~a\n~a~a"
    desc
    (make-string indentation #\space)
    (stylish-expr->string
      (match xs
        [(list x) (stylish-value->expr x)]
        [xs `(values ,@(map stylish-value->expr xs))])
      #:left indentation
      #:columns (- (current-stylish-print-columns)
                   (* 2 (current-debug-depth))
                   indentation))))

(define (values->string xs)
  (match xs
    [(list x) (format "~v" x)]
    [_ (format "(values~a)"
         (apply string-append
           (for/list {[x (in-list xs)]}
             (format " ~v" x))))]))

(define (call-with-debug-frame desc-str thunk)
  (define (enter) (dprintf #:prefix ">> " "~a" desc-str))
  (define (exit) (dprintf #:prefix "<< " "~a" desc-str))
  (define (work)
    (parameterize {[current-debug-depth (add1 (current-debug-depth))]}
      (thunk)))
  (dynamic-wind enter work exit))

(define (dprintf #:prefix [prefix "| "] fmt . args)
  (eprintf "~a"
    (indent prefix
      (apply format fmt args))))

(define (stylish-dprintf
          #:prefix [prefix "| "]
          #:columns [columns (current-stylish-print-columns)]
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          fmt . args)
  (dprintf #:prefix prefix "~a"
    (apply stylish-format
      #:columns (- columns (indent-length) (string-length prefix))
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

(define current-debug-depth
  (make-parameter 0))
