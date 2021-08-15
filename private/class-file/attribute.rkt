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
    [("StackMapTable") attribute:stack-map-table%]
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
    [("RuntimeVisibleAnnotations") attribute:runtime-visible-annotations%]
    [("RuntimeInvisibleAnnotations") attribute:runtime-invisible-annotations%]
    [("RuntimeVisibleParameterAnnotations") attribute:runtime-visible-parameter-annotations%]
    [("RuntimeInvisibleParameterAnnotations") attribute:runtime-invisible-parameter-annotations%]
    [("RuntimeVisibleTypeAnnotations") attribute:runtime-visible-type-annotations%]
    [("RuntimeInvisibleTypeAnnotations") attribute:runtime-invisible-type-annotations%]
    [("AnnotationDefault") attribute:annotation-default%]
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

(define-binary-class/length-prefixed attribute:stack-map-table% attribute%
  #:length u4
  ([entries (length-prefixed-array u2 stack-map-frame%)])
  (inspect #f))

(define-binary-class verification-type%
  ([tag u1])
  #:dispatch
  (case tag
    [(0 1 2 3 4 5 6) verification-type:empty%]
    [(7) verification-type:object%]
    [(8) verification-type:uninitialized%]
    [else (error "invalid verification type:" tag)])
  (inspect #f))

(define-binary-class verification-type:empty% verification-type%
  ()
  (inspect #f))

(define-binary-class verification-type:object% verification-type%
  ([cpool-index u2])
  (inspect #f))

(define-binary-class verification-type:uninitialized% verification-type%
  ([offset u2])
  (inspect #f))

(define-binary-class stack-map-frame%
  ([frame-type u1])
  #:dispatch
  (cond
    [(<= 0 frame-type 63) stack-map-frame:same%]
    [(<= 64 frame-type 127) stack-map-frame:same-locals-1-stack-item%]
    [(= 247 frame-type) stack-map-frame:same-locals-1-stack-item-extended%]
    [(<= 248 frame-type 250) stack-map-frame:chop%]
    [(= 251 frame-type) stack-map-frame:same-extended%]
    [(<= 252 frame-type 254) stack-map-frame:append%]
    [(= 255 frame-type) stack-map-frame:full%]
    [else (error "stack-map-frame: invalid frame type:" frame-type)])
  (inspect #f))

(define-binary-class stack-map-frame:same% stack-map-frame%
  ()
  (inspect #f))

(define-binary-class stack-map-frame:same-locals-1-stack-item% stack-map-frame%
  ([stack (array 1 verification-type%)])
  (inspect #f))

(define-binary-class stack-map-frame:same-locals-1-stack-item-extended% stack-map-frame%
  ([offset-delta u2]
   [stack (array 1 verification-type%)])
  (inspect #f))

(define-binary-class stack-map-frame:chop% stack-map-frame%
  ([offset-delta u2])
  (inspect #f))

(define-binary-class stack-map-frame:same-extended% stack-map-frame%
  ([offset-delta u2])
  (inspect #f))

(define-binary-class stack-map-frame:append% stack-map-frame%
  ([offset-delta u2]
   [locals (array (- frame-type 251) verification-type%)])
  (inspect #f))

(define-binary-class stack-map-frame:full% stack-map-frame%
  ([offset-delta u2]
   [locals (length-prefixed-array u2 verification-type%)]
   [stack (length-prefixed-array u2 verification-type%)])
  (inspect #f))

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

(define-binary-class annotation%
  ([type-index u2]
   [element-value-pairs (length-prefixed-array u2 element-value-pair%)])
  (inspect #f))

(define-binary-class element-value-pair%
  ([element-name-index u2]
   [value element-value%])
  (inspect #f))

(define-binary-class element-value%
  ([tag u1])
  #:dispatch
  (case (integer->char tag)
    [(#\B #\C #\D #\F #\I #\J #\S #\Z #\s) element-value:const%]
    [(#\e) element-value:enum-const%]
    [(#\c) element-value:class%]
    [(#\@) element-value:annotation%]
    [(#\[) element-value:array%]
    [else (error "invalid element-value tag:" tag)])
  (inspect #f))

(define-binary-class element-value:const% element-value%
  ([const-value-index u2])
  (inspect #f))

(define-binary-class element-value:enum-const% element-value%
  ([type-name-index u2]
   [const-name-index u2])
  (inspect #f))

(define-binary-class element-value:class% element-value%
  ([class-info-index u2])
  (inspect #f))

(define-binary-class element-value:annotation% element-value%
  ([annotation-value annotation%])
  (inspect #f))

(define-binary-class element-value:array% element-value%
  ([values (length-prefixed-array u2 element-value%)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:runtime-visible-annotations% attribute%
  #:length u4
  ([annotations (length-prefixed-array u2 annotation%)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:runtime-invisible-annotations% attribute%
  #:length u4
  ([annotations (length-prefixed-array u2 annotation%)])
  (inspect #f))

(define-binary-class parameter-annotation%
  ([annotations (length-prefixed-array u2 annotation%)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:runtime-visible-parameter-annotations% attribute%
  #:length u4
  ([parameter-annotations (length-prefixed-array u1 parameter-annotation%)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:runtime-invisible-parameter-annotations% attribute%
  #:length u4
  ([parameter-annotations (length-prefixed-array u1 parameter-annotation%)])
  (inspect #f))

(define-binary-class type-annotation%
  ([target target%]
   [target-path type-path%]
   [type-index u2]
   [element-value-pairs (length-prefixed-array u2 element-value-pair%)])
  (inspect #f))

(define-binary-class target%
  ([target-type u1])
  #:dispatch
  (case target-type
    [(#x00 #x01) target:type-parameter%]
    [(#x10) target:supertype%]
    [(#x11 #x12) target:type-parameter-bound%]
    [(#x13 #x14 #x15) target:empty%]
    [(#x16) target:formal-parameter%]
    [(#x17) target:throws%]
    [(#x40 #x41) target:localvar%]
    [(#x42) target:catch%]
    [(#x43 #x44 #x45 #x46) target:offset%]
    [(#x47 #x48 #x49 #x4A #x4B) target:type-argument%]
    [else (error "invalid target type:" target-type)])
  (inspect #f))

(define-binary-class target:type-parameter% target%
  ([type-parameter-index u1])
  (inspect #f))

(define-binary-class target:supertype% target%
  ([supertype-index u1])
  (inspect #f))

(define-binary-class target:type-parameter-bound% target%
  ([type-parameter-index u1]
   [bound-index u1])
  (inspect #f))

(define-binary-class target:empty% target%
  ()
  (inspect #f))

(define-binary-class target:formal-parameter% target%
  ([formal-parameter-index u1])
  (inspect #f))

(define-binary-class target:throws% target%
  ([throws-type-index u2])
  (inspect #f))

(define-binary-class target:localvar% target%
  ([table (length-prefixed-array u2 localvar-target-entry%)])
  (inspect #f))

(define-binary-class localvar-target-entry%
  ([start-pc u2]
   [length u2]
   [index u2])
  (inspect #f))

(define-binary-class target:catch% target%
  ([exception-table-index #f])
  (inspect #f))

(define-binary-class target:offset% target%
  ([offset u2])
  (inspect #f))

(define-binary-class target:type-argument% target%
  ([offset u2]
   [type-argument-index u1])
  (inspect #f))

(define-binary-class type-path%
  ([path (length-prefixed-array u1 type-path-element%)])
  (inspect #f))

(define-binary-class type-path-element%
  ([type-path-kind u1]
   [type-argument-index u1])
  (inspect #f))

(define-binary-class/length-prefixed attribute:runtime-visible-type-annotations% attribute%
  #:length u4
  ([annotations (length-prefixed-array u2 type-annotation%)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:runtime-invisible-type-annotations% attribute%
  #:length u4
  ([annotations (length-prefixed-array u2 type-annotation%)])
  (inspect #f))

(define-binary-class/length-prefixed attribute:annotation-default% attribute%
  #:length u4
  ([default-value element-value%])
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
