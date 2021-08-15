#lang racket/base

(require racket/class
         binary-class
         "binary.rkt")

(provide (all-defined-out))

(define-binary-class constant%
  ([tag u1])
  #:dispatch
  (case tag
    [(1) constant:utf8%]
    [(3) constant:integer%]
    [(4) constant:float%]
    [(5) constant:long%]
    [(6) constant:double%]
    [(7) constant:class%]
    [(8) constant:string%]
    [(9) constant:field-ref%]
    [(10) constant:method-ref%]
    [(11) constant:interface-method-ref%]
    [(12) constant:name-and-type%]
    [(15) constant:method-handle%]
    [(16) constant:method-type%]
    [(18) constant:invoke-dynamic%]
    [else (error "invalid tag: " tag)])
  (inspect #f))

(define-binary-class constant:utf8% constant%
  ([value (length-prefixed-modified-utf8-string u2)])
  (inspect #f))

(define-binary-class constant:integer% constant%
  ([value u4])
  (inspect #f))

(define-binary-class constant:float% constant%
  ([value float-be])
  (inspect #f))

(define-binary-class constant:long% constant%
  ([value (integer-be 8)])
  (inspect #f))

(define-binary-class constant:double% constant%
  ([value double-be])
  (inspect #f))

(define-binary-class constant:class% constant%
  ([name-index u2])
  (inspect #f))

(define-binary-class constant:string% constant%
  ([string-index u2])
  (inspect #f))

(define-binary-class constant:ref% constant%
  ([class-index u2]
   [name-and-type-index u2])
  (inspect #f))

(define-binary-class constant:field-ref% constant:ref%
  ()
  (inspect #f))

(define-binary-class constant:method-ref% constant:ref%
  ()
  (inspect #f))

(define-binary-class constant:interface-method-ref% constant:ref%
  ()
  (inspect #f))

(define-binary-class constant:name-and-type% constant%
  ([name-index u2]
   [type-index u2])
  (inspect #f))

(define-binary-class constant:method-handle% constant%
  ([kind u1]
   [index u2])
  (inspect #f))

(define-binary-class constant:method-type% constant%
  ([descriptor-index u2])
  (inspect #f))

(define-binary-class constant:invoke-dynamic% constant%
  ([bootstrap-method-attr-index u2]
   [name-and-type-index u2])
  (inspect #f))
