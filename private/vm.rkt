#lang racket/base

(provide (all-defined-out))

(define current-jump (make-parameter 'current-jump))

(define current-locals (make-parameter 'current-locals))

(define current-resolve-class (make-parameter 'current-resolve-class))

(define current-resolve-method (make-parameter 'current-resolve-method))

(define current-return (make-parameter 'current-return))
