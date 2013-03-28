#lang racket/base

(provide
  keyword->symbol
  symbol->keyword
  keyword-upcase
  keyword-downcase
  keyword-titlecase
  keyword-foldcase
  keyword=?
  keyword<?
  keyword>?
  keyword<=?
  keyword>=?
  format-keyword)

(require
  (only-in racket/syntax format-symbol))

(define (keyword->symbol k)
  (string->symbol (keyword->string k)))

(define (symbol->keyword k)
  (string->keyword (symbol->string k)))

(define (keyword-upcase sym)
  (string->keyword
    (string-upcase
      (keyword->string sym))))

(define (keyword-downcase sym)
  (string->keyword
    (string-downcase
      (keyword->string sym))))

(define (keyword-titlecase sym)
  (string->keyword
    (string-titlecase
      (keyword->string sym))))

(define (keyword-foldcase sym)
  (string->keyword
    (string-foldcase
      (keyword->string sym))))

(define (keyword=? one two)
  (eq? one two))

(define (keyword>? one two)
  (and (not (eq? one two))
    (string>?
      (keyword->string one)
      (keyword->string two))))

(define (keyword<=? one two)
  (or (eq? one two)
    (string<=?
      (keyword->string one)
      (keyword->string two))))

(define (keyword>=? one two)
  (or (eq? one two)
    (string>=?
      (keyword->string one)
      (keyword->string two))))

(define (format-keyword fmt . args)
  (symbol->keyword (apply format-symbol fmt args)))
