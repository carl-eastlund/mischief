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
  (only-in racket/bool symbol=?)
  (only-in racket/syntax format-symbol))

(define (symbol-upcase sym)
  (string->symbol
    (string-upcase
      (symbol->string sym))))

(define (symbol-downcase sym)
  (string->symbol
    (string-downcase
      (symbol->string sym))))

(define (symbol-titlecase sym)
  (string->symbol
    (string-titlecase
      (symbol->string sym))))

(define (symbol-foldcase sym)
  (string->symbol
    (string-foldcase
      (symbol->string sym))))

(define (symbol<? one two)
  (string<?
    (symbol->string one)
    (symbol->string two)))

(define (symbol>? one two)
  (string>?
    (symbol->string one)
    (symbol->string two)))

(define (symbol<=? one two)
  (string<=?
    (symbol->string one)
    (symbol->string two)))

(define (symbol>=? one two)
  (string>=?
    (symbol->string one)
    (symbol->string two)))
