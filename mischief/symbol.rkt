#lang racket/base

(provide
  symbol-upcase
  symbol-downcase
  symbol-titlecase
  symbol-foldcase
  symbol=?
  symbol<?
  symbol>?
  symbol<=?
  symbol>=?
  format-symbol)

(require
  mischief/define
  (only-in racket/bool symbol=?)
  (only-in racket/syntax format-symbol))

(define-if-unbound (symbol-upcase sym)
  (string->symbol
    (string-upcase
      (symbol->string sym))))

(define-if-unbound (symbol-downcase sym)
  (string->symbol
    (string-downcase
      (symbol->string sym))))

(define-if-unbound (symbol-titlecase sym)
  (string->symbol
    (string-titlecase
      (symbol->string sym))))

(define-if-unbound (symbol-foldcase sym)
  (string->symbol
    (string-foldcase
      (symbol->string sym))))

(define-if-unbound (symbol<? one two)
  (and (not (eq? one two))
    (string<?
      (symbol->string one)
      (symbol->string two))))

(define-if-unbound (symbol>? one two)
  (and (not (eq? one two))
    (string>?
      (symbol->string one)
      (symbol->string two))))

(define-if-unbound (symbol<=? one two)
  (or (eq? one two)
    (string<=?
      (symbol->string one)
      (symbol->string two))))

(define-if-unbound (symbol>=? one two)
  (or (eq? one two)
    (string>=?
      (symbol->string one)
      (symbol->string two))))
