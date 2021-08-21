#lang racket/base

(require racket/list
         racket/match)

(provide (all-defined-out))

(define (array-type? v)
  (match v
    [(cons 'array (? nonmethod-type?)) #t]
    [_ #f]))

(define (class-type? v)
  (match v
    [(cons 'class (? string?)) #t]
    [_ #f]))

(define (method-type? v)
  (match v
    [(list 'method (list (? nonmethod-type? ) ...) (? return-type?)) #t]
    [_ #f]))

(define (nonmethod-type? v)
  (or (array-type? v)
      (class-type? v)
      (primitive-type? v)))

(define (primitive-type? v)
  (and (memq v '(byte short int long char fload double boolean)) #t))

(define (return-type? v)
  (or (nonmethod-type? v)
      (eq? 'void v)))

(define (type? v)
  (or (method-type? v)
      (nonmethod-type? v)))

(define (method-arg-types t)
  (second t))

(define (method-arg-count t)
  (length (method-arg-types t)))

(define (method-return-type t)
  (third t))
