#lang racket/base

(provide (all-defined-out))

(define current-locals (make-parameter 'current-locals))

(define current-resolve-method (make-parameter 'current-resolve-method))

(define current-return (make-parameter 'current-return))
