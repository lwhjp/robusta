#lang racket/base

(require racket/class
         racket/provide
         binary-class
         "attribute.rkt"
         "constant.rkt"
         "binary.rkt")

(provide (matching-identifiers-out #px"^[^\\*].*%$" (all-defined-out)))

; TODO: verify

(define class-file-magic
  (bytes->immutable-bytes (bytes #xCA #xFE #xBA #xBE)))

(define class-access-flags
  (flags u2 '(public
              [final #x0010]
              super
              [interface #x0200]
              abstract
              [synthetic #x1000]
              annotation
              enum)))

(define-binary-class *class-file%
  ([_ (constant class-file-magic)]
   [major-version u2]
   [minor-version u2]
   [constants (length-prefixed-array u2 constant% 1)]
   [access-flags class-access-flags]
   [this-class u2]
   [super-class u2]
   [interfaces (length-prefixed-array u2 u2)]
   [fields (length-prefixed-array u2 field%)]
   [methods (length-prefixed-array u2 method%)]
   [attributes (length-prefixed-array u2 attribute%)])
  (inspect #f))

(define class-file%
  (class *class-file%
    (super-new)
    (inspect #f)
    ; Hack so that we can get constants while reading
    (define/override (read in 
                           [args null]
                           [skip-dispatch? #f]
                           [skip-super-class #f])
      (parameterize ([current-class-file this])
        (super read in args skip-dispatch? skip-super-class)))))

(define field-access-flags
  (flags u2 '(public
              private
              protected
              static
              final
              [volatile #x0040]
              transient
              [synthetic #x1000]
              [enum #x4000])))

(define-binary-class field%
  ([access-flags field-access-flags]
   [name-index u2]
   [descriptor-index u2]
   [attributes (length-prefixed-array u2 attribute%)])
  (inspect #f))

(define method-access-flags
  (flags u2 '(public
              private
              protected
              static
              final
              synchronized
              bridge
              varargs
              native
              [abstract #x0400]
              strict
              synthetic)))

(define-binary-class method%
  ([access-flags method-access-flags]
   [name-index u2]
   [descriptor-index u2]
   [attributes (length-prefixed-array u2 attribute%)])
  (inspect #f))
