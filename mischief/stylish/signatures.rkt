#lang racket

(provide
  expression^
  expr-style^
  print^
  print-style^
  format^
  stylish^

  gen:stylish-writable
  stylish-writable?
  generic-stylish-write

  gen:stylish-printable
  stylish-printable?
  generic-stylish-value->expr
  generic-stylish-quotable?)

(require
  (for-syntax
    syntax/parse)
  racket/generic)

(define-generics stylish-writable
  (generic-stylish-write stylish-writable port style))

(define-generics stylish-printable
  (generic-stylish-value->expr stylish-printable style)
  (generic-stylish-quotable? stylish-printable style))

(define-signature expression^
  [value->expression
   value-quotable?

   expr-style?
   empty-expr-style
   simple-expr-style
   extend-expr-style
   set-expr-style-default-convert

   expr-style-extension?
   expr-style-extension])

(define-signature expr-style^
  [default-expr-style
   current-expr-style])

(define-signature print^
  [print-to-stylish-port
   print-expression
   print-separator

   stylish-port?

   print-style?
   empty-print-style
   simple-print-style
   extend-print-style
   set-print-style-default-printer

   print-style-extension?
   print-style-extension])

(define-signature print-style^
  [default-print-style
   current-print-style])

(define-signature format^
  [print-formatted])

(define-signature stylish^

  [(define-syntaxes {with-stylish-port}
     (syntax-parser
       [(_ body:expr ...+)
        #'(call-with-stylish-port (current-output-port)
            (lambda (port)
              (parameterize {[current-output-port port]}
                body ...)))]))

   (struct stylish-comment-expr {comment expr})
   (struct stylish-unprintable-expr {name})

   stylish-print-handler

   stylish-format
   stylish-printf

   stylish-print
   stylish-println
   stylish-print-as-string

   stylish-write
   stylish-writeln
   stylish-write-as-string

   stylish-print-separator
   call-with-stylish-port

   stylish-quotable-value?
   stylish-value->expr

   current-stylish-print-columns])
