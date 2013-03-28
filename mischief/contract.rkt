#lang racket/base

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  (for-syntax
    racket/base
    syntax/parse)
  racket/dict
  racket/match
  racket/contract
  racket/pretty
  mischief/define
  syntax/srcloc)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide
  define-flat-rec-contract
  predicate/c)
(provide
  (contract-out
    [dict/c (-> contract? contract? contract?)]
    [nat/c flat-contract?]
    [type-predicate/c
     (->*
         {}
         {contract?
          #:strict? boolean?
          #:super (or/c (-> any/c any/c) #false)}
       contract?)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic Configuration

(define (show-blame-error blame value message)
  (string-append
   "Contract Violation!\n"
   (format "Guilty Party: ~a\n" (blame-positive blame))
   (format "Innocent Party: ~a\n" (blame-negative blame))
   (format "Contracted Value Name: ~a\n" (blame-value blame))
   (format "Contract Location: ~a\n"
     (if (source-location-known? (blame-source blame))
       (source-location->string (blame-source blame))
       "<unknown>"))
   (format "Contract Name: ~a\n" (blame-contract blame))
   (format "Offense: ~a\n" message)
   (format "Offending Value:\n~a\n"
     (pretty-format value))))

#;(current-blame-format show-blame-error)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation

(define-syntax (define-flat-rec-contract stx)
  (syntax-parse stx
    [(_ name:id alt:expr ...)
     #'(define name
         (flat-rec-contract name alt ...))]))

(define-if-unbound predicate/c
  (-> any/c boolean?))

(define nat/c natural-number/c)

(define (type-predicate/c [input/c any/c]
          #:strict? [strict? #true]
          #:super [super #false])
  (type/c input/c strict? super))

(define type-predicate-contract-property
  (build-contract-property
    #:name
    (lambda (c)
      (define input/c (type/c-input/c c))
      (define strict? (type/c-strict? c))
      (define super (type/c-super c))
      `(type-predicate/c ,(contract-name input/c)
         ,@(if strict? '[] '[#:strict? #false])
         ,@(if super `[#:super ,(contract-name super)] '[])))
    #:first-order
    (lambda (c)
      type-predicate/first-order?)
    #:projection
    (lambda (c)
      (define input/c (type/c-input/c c))
      (define strict? (type/c-strict? c))
      (define super (type/c-super c))
      (lambda (b)
        (define ~b (blame-swap b))
        (lambda (f)
          (unless (procedure? f)
            (raise-blame-error b f
              "expected ~a, but got ~a: ~v"
              "a procedure of 1 argument"
              "a non-procedure"
              f))
          (unless (procedure-arity-includes? f 1)
            (raise-blame-error b f
              "expected ~a, but got ~a: ~v"
              "a procedure of 1 argument"
              "a procedure that does not accept 1 argument"
              f))
          (lambda (x0)
            (define x (((contract-projection input/c) ~b) x0))
            (define y (f x))
            (when strict?
              (unless (boolean? y)
                (raise-blame-error b y
                  "expected ~a, but got ~a: ~v"
                  "a boolean"
                  "a non-boolean"
                  y)))
            (when y
              (when super
                (unless (super x)
                  (raise-blame-error b f
                    "expected ~a, but got ~a; ~a"
                    (format "a predicate recognizing a subtype of ~a"
                      (object-name super))
                    (object-name f)
                    (format "counterexample ~v is ~a but ~a"
                      x
                      (format "accepted by ~a" (object-name f))
                      (format "rejected by ~a" (object-name super)))))))
            y))))))

(define (type-predicate/first-order? f)
  (and (procedure? f)
    (procedure-arity-includes? f 1)))

(struct type/c
  [input/c strict? super]
  #:reflection-name 'type-predicate/c
  #:property prop:contract type-predicate-contract-property)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contracted Dictionaries
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A CDict is (cdict (Listof (Cons Proj Proj)) Dict)
;; A Proj is (proj Contract Blame)
(struct cdict [projections bindings])
(struct proj [contract blame])

(define (dict/c key/c value/c)
  (let* ([key/c (coerce-contract 'dict/c key/c)]
         [value/c (coerce-contract 'dict/c value/c)])
    (make-contract
      #:name (list 'dict/c (contract-name key/c) (contract-name value/c))
      #:first-order dict?
      #:projection
      (lambda (blame)
        (lambda (dict)
          (unless (dict? dict)
            (raise-blame-error blame dict
              "expected a dictionary, but got ~v"
              dict))
          (wrap
            (list* (cons (proj key/c blame) (proj value/c blame))
              (dict->projections dict))
            (dict->bindings dict)))))))

(define -ref
  (case-lambda
    [(dict key)
     (match dict
       [(cdict projs binds)
        (let* ([key (key-in projs key)])
          (value-out projs (dict-ref binds key)))])]
    [(dict key failure)
     (match dict
       [(cdict projs binds)
        (let* ([key (key-in projs key)])
          (let/ec return
            (define (fail)
              (return (if (procedure? failure) (failure) failure)))
            (value-out projs (dict-ref binds key fail))))])]))

(define (-set! dict key value)
  (match dict
    [(cdict projs binds)
     (dict-set! binds (key-in projs key) (value-in projs value))]))

(define (-set dict key value)
  (match dict
    [(cdict projs binds)
     (wrap projs (dict-set binds (key-in projs key) (value-in projs value)))]))

(define (-rem! dict key)
  (match dict
    [(cdict projs binds)
     (dict-remove! binds (key-in projs key))]))

(define (-rem dict key)
  (match dict
    [(cdict projs binds)
     (wrap projs (dict-remove binds (key-in projs key)))]))

(define (-size dict)
  (match dict
    [(cdict projs binds)
     (dict-count binds)]))

(define (-fst dict)
  (match dict
    [(cdict projs binds)
     (dict-iterate-first binds)]))

(define (-nxt dict iter)
  (match dict
    [(cdict projs binds)
     (dict-iterate-next binds iter)]))

(define (-key dict iter)
  (match dict
    [(cdict projs binds)
     (key-out projs (dict-iterate-key binds iter))]))

(define (-val dict iter)
  (match dict
    [(cdict projs binds)
     (value-out projs (dict-iterate-value binds iter))]))

(define (key-in projs key)
  (if (null? projs)
      key
      (key-in (cdr projs) (project-in (caar projs) key))))

(define (value-in projs value)
  (if (null? projs)
      value
      (value-in (cdr projs) (project-in (cdar projs) value))))

(define (key-out projs key)
  (if (null? projs)
      key
      (project-out (caar projs) (key-out (cdr projs) key))))

(define (value-out projs value)
  (if (null? projs)
      value
      (project-out (cdar projs) (value-out (cdr projs) value))))

(define (project-in p x)
  (match p
    [(proj c b)
     (((contract-projection c) (blame-swap b)) x)]))

(define (project-out p x)
  (match p
    [(proj c b)
     (((contract-projection c) b) x)]))

(define (dict->bindings dict)
  (match dict
    [(cdict projs binds) binds]
    [_ dict]))

(define (dict->projections dict)
  (match dict
    [(cdict projs binds) projs]
    [_ null]))

(define (wrap projs binds)
  ((dict->wrapper binds) projs binds))

(define (dict->wrapper dict)
  (if (dict-mutable? dict)
      (if (dict-can-functional-set? dict)
          (if (dict-can-remove-keys? dict) make-:!+- make-:!+_)
          (if (dict-can-remove-keys? dict) make-:!_- make-:!__))
      (if (dict-can-functional-set? dict)
          (if (dict-can-remove-keys? dict) make-:_+- make-:_+_)
          (if (dict-can-remove-keys? dict) make-:__- make-:___))))

;; The __- case (removal without functional or mutable update) is nonsensical.
(define prop:!+- (vector -ref -set! -set -rem! -rem -size -fst -nxt -key -val))
(define prop:!+_ (vector -ref -set! -set  #f    #f  -size -fst -nxt -key -val))
(define prop:!_- (vector -ref -set!  #f  -rem!  #f  -size -fst -nxt -key -val))
(define prop:!__ (vector -ref -set!  #f   #f    #f  -size -fst -nxt -key -val))
(define prop:_+- (vector -ref  #f   -set  #f   -rem -size -fst -nxt -key -val))
(define prop:_+_ (vector -ref  #f   -set  #f   -rem -size -fst -nxt -key -val))
(define prop:__- (vector -ref  #f    #f   #f    #f  -size -fst -nxt -key -val))
(define prop:___ (vector -ref  #f    #f   #f    #f  -size -fst -nxt -key -val))

;; The __- case (removal without functional or mutable update) is nonsensical.
(define-struct (:!+- cdict) [] #:property prop:dict prop:!+-)
(define-struct (:!+_ cdict) [] #:property prop:dict prop:!+_)
(define-struct (:!_- cdict) [] #:property prop:dict prop:!_-)
(define-struct (:!__ cdict) [] #:property prop:dict prop:!__)
(define-struct (:_+- cdict) [] #:property prop:dict prop:_+-)
(define-struct (:_+_ cdict) [] #:property prop:dict prop:_+_)
(define-struct (:__- cdict) [] #:property prop:dict prop:__-)
(define-struct (:___ cdict) [] #:property prop:dict prop:___)
