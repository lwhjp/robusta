#lang racket/base

(require racket/class
         racket/provide
         binary-class
         "binary.rkt"
         "constant.rkt")

(provide (matching-identifiers-out #rx"%$" (all-defined-out))
         current-class-file)

; class-file% will set this when reading
(define current-class-file (make-parameter #f))

(define (get-constant-string idx)
  (define c (vector-ref (get-field constants (current-class-file)) (sub1 idx)))
  (unless (is-a? c constant:utf8%)
    (error "invalid constant type: expected utf8"))
  (get-field value c))

(define-syntax-rule
  (define-binary-class/length-prefixed class% super%
    #:length length-type
    ([id type] ...)
    body ...)
  (define-binary-class class% super%
    ([(id ...) (verify-length-prefixed length-type type ...)])
    body ...))

(define-binary-class attribute%
  ([attribute-name-index u2])
  #:dispatch
  (case (get-constant-string attribute-name-index)
    [("ConstantValue") attribute:constant-value%]
    [("Code") attribute:code%]
    ; TODO: StackMapTable
    [("Exceptions") attribute:exceptions%]
    [("InnerClasses") attribute:inner-classes%]
    [("EnclosingMethod") attribute:enclosing-method%]
    [("Synthetic") attribute:synthetic%]
    [("Signature") attribute:signature%]
    [("SourceFile") attribute:source-file%]
    [("SourceDebugExtension") attribute:source-debug-extension%]
    [("LineNumberTable") attribute:line-number-table%]
    [("LocalVariableTable") attribute:local-variable-table%]
    [("LocalVariableTypeTable") attribute:local-variable-type-table%]
    [("Deprecated") attribute:deprecated%]
    ; RuntimeVisibleAnnotations
    ; RuntimeInvisibleAnnotations
    ; RuntimeVisibleParameterAnnotations
    ; RuntimeInvisibleParameterAnnotations
    ; RuntimeVisibleTypeAnnotations
    ; RuntimeInvisibleTypeAnnotations
    ; AnnotationDefault
    [("BootstrapMethods") attribute:bootstrap-methods%]
    [("MethodParameters") attribute:method-parameters%]
    [else unknown-attribute%])
  (inspect #f))

(define-binary-class/length-prefixed attribute:constant-value% attribute%
  #:length u4
  ([constant-value-index u2])
  (inspect #f))

(define-binary-class/length-prefixed attribute:code% attribute%
  #:length u4
  ([max-stack u2]
   [max-locals u2]
   [code (length-prefixed-bytestring u4)]
   [exception-table (length-prefixed-array u2 exception-table-entry%)]
   [attributes (length-prefixed-array u2 attribute%)])
  (inspect #f))

(define-binary-class exception-table-entry%
  ([start-pc u2]
   [end-pc u2]
   [handler-pc u2]
   [catch-type u2])
  (inspect #f))

; TODO: stack-map-table

(define-binary-class/length-prefixed attribute:exceptions% attribute%
  #:length u4
  ([exception-index-table (length-prefixed-array u2 u2)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:inner-classes% attribute%
  #:length u4
  ([classes (length-prefixed-array u2 inner-class%)])
  (inspect #f))

(define binary:inner-class-access-flags
  (flags u2 '(public
              private
              protected
              static
              final
              [interface #x0200]
              abstract
              [synthetic #x1000]
              annotation
              enum)))

(define-binary-class inner-class%
  ([inner-class-info-index u2]
   [outer-class-info-index u2]
   [inner-name-index u2]
   [inner-class-access-flags binary:inner-class-access-flags])
  (inspect #f))

(define-binary-class/length-prefixed attribute:enclosing-method% attribute%
  #:length u4
  ([class-index u2]
   [method-index u2])
  (inspect #f))

(define-binary-class attribute:synthetic% attribute%
  ([_ (constant u4 0)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:signature% attribute%
  #:length u4
  ([signature-index u2])
  (inspect #f))

(define-binary-class/length-prefixed attribute:source-file% attribute%
  #:length u4
  ([source-file-index u2])
  (inspect #f))

(define-binary-class attribute:source-debug-extension% attribute%
  ([debug-extension (length-prefixed-modified-utf8-string u4)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:line-number-table% attribute%
  #:length u4
  ([line-number-table (length-prefixed-array u2 line-number%)])
  (inspect #f))

(define-binary-class line-number%
  ([start-pc u2]
   [line-number u2])
  (inspect #f))

(define-binary-class/length-prefixed attribute:local-variable-table% attribute%
  #:length u4
  ([local-variable-table (length-prefixed-array u2 local-variable%)])
  (inspect #f))

(define-binary-class local-variable%
  ([start-pc u2]
   [length u2]
   [name-index u2]
   [descriptor-index u2]
   [index u2])
  (inspect #f))

(define-binary-class/length-prefixed attribute:local-variable-type-table% attribute%
  #:length u4
  ([local-variable-type-table (length-prefixed-array u2 local-variable-type%)])
  (inspect #f))

(define-binary-class local-variable-type%
  ([start-pc u2]
   [length u2]
   [name-index u2]
   [signature-index u2]
   [index u2])
  (inspect #f))

(define-binary-class attribute:deprecated% attribute%
  ([_ (constant u4 0)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:bootstrap-methods% attribute%
  #:length u4
  ([bootstrap-methods (length-prefixed-array u2 bootstrap-method%)])
  (inspect #f))

(define-binary-class bootstrap-method%
  ([bootstrap-method-ref u2]
   [bootstrap-arguments (length-prefixed-array u2 u2)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:method-parameters% attribute%
  #:length u4
  ([parameters (length-prefixed-array u1 parameter%)])
  (inspect #f))

(define parameter-access-flags
  (flags u2 '([final #x0010]
              [synthetic #x1000]
              [mandated #x8000])))

(define-binary-class parameter%
  ([name-index u2]
   [access-flags parameter-access-flags])
  (inspect #f))

(define-binary-class unknown-attribute% attribute%
  ([info (length-prefixed-bytestring u4)])
  (inspect #f))
