#lang racket/base

(provide
  (all-defined-out))

(require
  racket/pretty
  racket/port
  racket/list
  racket/block
  mischief/list
  mischief/struct
  mischief/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression to String

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A RenderTable is:
;;  (render-table [Listof RenderExt] (Or (Any OutputPort -> Void) #false))
;; A RenderExt is, for some type T:
;;  (render-extension [Predicate T] [Render T])
;; A [Predicate T] is (Any -> Boolean : T) ;; same as above
;; A [Render T] is (T OutputPort Indent RecRender -> Void)
;; An Indent is (-> Void)
;; A RecRender is: (Any Pos Nat Nat -> Void)
;; A Pos is 'only, 'first, 'middle, or 'last
(struct render-table [extensions default])
(struct render-extension [predicate render])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions

(define (format-expression e [et (current-render-expression-table)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (pretty-print-columns)]
          #:newline? [newline? #t])
  (call-with-output-string
    (lambda (port)
      (render-expr 'format-expression e et
        port left right columns (make-hasheq))
      (when newline?
        (newline port)))))

(define (print-expression e [et (current-render-expression-table)]
          #:port [port (current-output-port)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (pretty-print-columns)]
          #:newline? [newline? #t])
  (render-expr 'print-expression e et
    port left right columns (make-hasheq))
  (when newline?
    (newline port)))

(define (render-expression-extension? x)
  (render-extension? x))

(define (render-expression-extension type? render)
  (render-extension type? render))

(define (render-expression-table? x)
  (render-table? x))

(define (extend-render-expression-table et
          #:default [default (render-table-default et)]
          #:after? [after? #false]
          . extensions)
  (render-table
    (if after?
      (append (render-table-extensions et) extensions)
      (append extensions (render-table-extensions et)))
    default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Functions

(define (render-expr name e et port left right columns cache:expr-length)
  (match! et
    [(render-table extensions default)
     (define (len e)
       (expr-length name e et cache:expr-length))
     (let render/expr {[e e] [left left] [right right]}
       (if (<= (len e) (- columns left right))
         (render/simple name e et port)
         (let render/extensions {[extensions extensions]}
           (if (empty? extensions)
             (if (procedure? default)
               (default e port)
               (cannot-render-error name e))
             (match! (first extensions)
               [(render-extension type? render)
                (if (type? e)
                  (block
                    (define margin (+ left indentation))
                    (define (indent)
                      (newline port)
                      (for {[i (in-range margin)]}
                        (space port)))
                    (define (render/recur e pos l r)
                      (unless (member? pos render-positions)
                        (bad-position-error name pos e))
                      (define first? (memq? pos '[only first]))
                      (define last? (memq? pos '[only last]))
                      (render/expr e
                        (if first? (+ left l) (+ left l indentation))
                        (if last? (+ right r) 0)))
                    (render e port indent render/recur))
                  (render/extensions (rest extensions)))])))))]))

(define (render/simple name e et port)
  (match! et
    [(render-table extensions default)
     (let render/expr {[e e]}
       (let render/extensions {[extensions extensions]}
         (if (empty? extensions)
           (if (procedure? default)
             (default e port)
             (cannot-render-error name e))
           (match! (first extensions)
             [(render-extension type? render)
              (if (type? e)
                (render e port
                  (lambda () (space port))
                  (lambda (e pos l r)
                    (render/expr e)))
                (render/extensions (rest extensions)))]))))]))

(define (expr-length name e et cache:expr-length)
  (match! et
    [(render-table extensions default)
     (let len/expr {[e e]}
       (hash-ref! cache:expr-length e
         (lambda ()
           (let len/extensions {[extensions extensions]}
             (if (empty? extensions)
               (if (procedure? default)
                 (string-length
                   (call-with-output-string
                     (lambda (port)
                       (default e port))))
                 (cannot-render-error name e))
               (match! (first extensions)
                 [(render-extension type? render)
                  (if (type? e)
                    (block
                      (define recorded-length (box 0))
                      (define (record! n)
                        (set-box! recorded-length
                          (+ (unbox recorded-length) n)))
                      (define (separate) (record! 1))
                      (define (render/recur e pos l r)
                        (unless (member? pos render-positions)
                          (bad-position-error name pos e))
                        (record! (len/expr e)))
                      (define delimiters
                        (call-with-output-string
                          (lambda (port)
                            (render e port separate render/recur))))
                      (+ (string-length delimiters)
                         (unbox recorded-length)))
                    (len/extensions (rest extensions)))]))))))]))

(define indentation 1)

(define (space [port (current-output-port)])
  (write-char #\space port))

(define render-positions
  '[only first middle last])

(define (cannot-render-error name e)
  (error name
    "cannot render expression: ~v" e))

(define (bad-position-error name pos e)
  (error name
    "bad expression position ~v, expected one of ~v; expression was: ~v"
    pos render-positions e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression to String Extensions

;;;;;;;;;;;;;;;;;;;;
;; Render Special

(define special-prefix-table
  (hash
    'quote "'"
    'quasiquote "`"
    'unquote ","
    'unquote-splicing ",@"
    'syntax "#'"
    'quasisyntax "#`"
    'unsyntax "#,"
    'unsyntax-splicing "#,@"))

(define (special? x)
  (match! x
    [(list keyword _)
     (hash-has-key? special-prefix-table keyword)]
    [_ #false]))

(define (special-keyword sp) (first sp))
(define (special-contents sp) (second sp))
(define (special-prefix sp)
  (hash-ref special-prefix-table
    (special-keyword sp)))

(define (render-special sp port sep render)
  (define prefix (special-prefix sp))
  (write-string prefix port)
  (render (special-contents sp) 'only (string-length prefix) 0))

(define render-special-extension
  (render-extension special?
    render-special))

;;;;;;;;;;;;;;;;;;;;
;; Render List

(define (write-with-parens port thunk)
  (write-string "(" port)
  (thunk)
  (write-string ")" port))

(define (render-list xs port sep render)
  (write-with-parens port
    (lambda ()
      (cond
        [(empty? xs) (void)]
        [(empty? (rest xs))
         (render (first xs) 'only 1 1)]
        [else
         (render (first xs) 'first 1 1)
         (let render-rest {[xs (rest xs)]}
           (sep)
           (if (empty? (rest xs))
             (render (first xs) 'last 1 1)
             (begin
               (render (first xs) 'middle 1 1)
               (render-rest (rest xs)))))]))))

(define render-list-extension
  (render-extension list?
    render-list))

;;;;;;;;;;;;;;;;;;;;
;; Render Improper List

(define (render-list* x port sep render)
  (write-with-parens port
    (lambda ()
      (render (car x) 'first 1 1)
      (let render-cdr {[x (cdr x)]}
        (sep)
        (if (pair? x)
          (begin
            (render (car x) 'middle 1 1)
            (render-cdr (cdr x)))
          (begin
            (write-string ". " port)
            (render x 'last 3 1)))))))

(define render-list*-extension
  (render-extension pair?
    render-list*))

;;;;;;;;;;;;;;;;;;;;
;; Render Vector

(define (render-vector vec port render sep)
  (write-string "#" port)
  (render (vector->list vec) 'only 1 0))

(define render-vector-extension
  (render-extension vector?
    render-vector))

;;;;;;;;;;;;;;;;;;;;
;; Render Hash

(define (render-hash ht port sep render)
  (define prefix
    (cond
      [(hash-eq? ht) "#hasheq"]
      [(hash-eqv? ht) "#hasheqv"]
      [(hash-equal? ht) "#hash"]))
  (write-string prefix port)
  (render (hash->list ht) 'only (string-length prefix) 0))

(define render-hash-extension
  (render-extension hash?
    render-hash))

;;;;;;;;;;;;;;;;;;;;
;; Render Box

(define (render-box b port sep render)
  (write-string "#&" port)
  (render (unbox b) 'only 2 0))

(define render-box-extension
  (render-extension box?
    render-box))

;;;;;;;;;;;;;;;;;;;;
;; Render Prefab

(define (render-prefab x port sep render)
  (write-string "#s" port)
  (render (cons (prefab-key x) (prefab-fields x)) 'only 2 0))

(define render-prefab-extension
  (render-extension prefab?
    render-prefab))

;;;;;;;;;;;;;;;;;;;;
;; Render Atoms

(define (render-atom x port sep render)
  (write x port))

(define render-symbol-extension
  (render-extension symbol? render-atom))
(define render-keyword-extension
  (render-extension keyword? render-atom))
(define render-string-extension
  (render-extension string? render-atom))
(define render-bytes-extension
  (render-extension bytes? render-atom))
(define render-number-extension
  (render-extension number? render-atom))
(define render-boolean-extension
  (render-extension boolean? render-atom))
(define render-regexp-extension
  (render-extension regexp? render-atom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression to String Tables

(define empty-render-expression-table
  (render-table empty #false))

(define default-render-expression-table
  (render-table
    (list
      render-special-extension
      render-list-extension
      render-list*-extension
      render-vector-extension
      render-hash-extension
      render-box-extension
      render-prefab-extension
      render-symbol-extension
      render-keyword-extension
      render-string-extension
      render-bytes-extension
      render-number-extension
      render-boolean-extension
      render-regexp-extension)
    write))

(define current-render-expression-table
  (make-parameter default-render-expression-table))
