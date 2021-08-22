#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/stx)
         racket/class
         racket/function
         racket/list
         racket/provide
         racket/splicing
         racket/stxparam
         "type.rkt"
         "vm.rkt")

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^:" name)
                 (substring name 1)))
          (all-defined-out)))

(define (local-variable index)
  (vector-ref (current-locals) index))

(define (set-local-variable! index v)
  (vector-set! (current-locals) index v))

(define (pop t stack)
  (case t
    [(double long) (values (car stack) (cddr stack))]
    [else (values (car stack) (cdr stack))]))

(define (push t v stack)
  (case t
    [(double long) (list* v 'pad stack)]
    [else (list* v stack)]))

(define-syntax (generate/template stx)
  (syntax-case stx ()
    [(_ ([var (val ...)] ...) template)
     (with-syntax ([((subst ...) ...) (apply stx-map list (syntax->list #'((val ...) ...)))])
       #'(splicing-let-syntax
             ([go (λ (stx)
                    (syntax-case stx ()
                      [(_ arg (... ...))
                       (with-syntax ([(var ...) #'(arg (... ...))])
                         #'template)]))])
           (go subst ...) ...))]))

(define-syntax-parameter this-type #f)

(define-for-syntax (generate-type-prefixed-ids base ts)
  (for/list ([t (in-list ts)])
    (define type-name (symbol->string (syntax-e t)))
    (define prefix-char (string-ref type-name 0))
    (format-id base ":~a~a" prefix-char base)))

(define-syntax (define/with-types stx)
  (syntax-case stx ()
    [(_ (t ...)
        ((base arg ...) stack)
        body ...)
     #`(generate/template ([type (t ...)]
                           [:t #,(for/list ([t (syntax->list #'(t ...))])
                                   (define type-name (symbol->string (syntax-e t)))
                                   (define prefix-char (string-ref type-name 0))
                                   (format-id #'base ":~a~a" prefix-char #'base))])
         (splicing-syntax-parameterize ([this-type (λ (stx) #''type)])
           (define ((:t arg ...) stack)
             body ...)))]))

(define-syntax (define/with-types+constants stx)
  (syntax-case stx ()
    [(_ (t ...) (c ...)
        ((base arg) stack)
        body ...)
     #`(generate/template ([type (t ...)]
                           [:t #,(for/list ([t (syntax->list #'(t ...))])
                                   (define type-name (symbol->string (syntax-e t)))
                                   (define prefix-char (string-ref type-name 0))
                                   (format-id #'base ":~a~a" prefix-char #'base))])
         (splicing-syntax-parameterize ([this-type (λ (stx) #''type)])
           (define/with-constants (c ...)
             ((:t arg) stack)
             body ...)))]))

(define-syntax (define/with-constants stx)
  (syntax-case stx ()
    [(_ (c ...)
        ((base arg) stack)
        body ...)
     #`(begin
         (define ((base arg) stack) body ...)
         (generate/template ([val (c ...)]
                             [name #,(map (λ (v) (format-id #'base "~a_~a" #'base v))
                                          (syntax->datum #'(c ...)))])
           (define (name) (base 'val))))]))

; Constants

(define ((:nop) stack) stack)

(define ((:aconst_null) stack) (push 'ref 'null stack))

; :iconst is not an instruction
(define/with-constants (0 1 2 3 4 5)
  ((:iconst v) stack)
  (push 'int v stack))

(define :iconst_m1 (:iconst -1))

(define/with-types+constants (long float double) (0 1)
  ((const v) stack)
  (push this-type v stack))

(define :fconst_2 (:fconst 2))

(define ((:bipush v) stack) (push 'byte v stack))

(define ((:sipush v) stack) (push 'short v stack))

(define ((:ldc v) stack) (cons v stack))

(define :ldc_w :ldc)

(define ((:ldc2_w v) stack) (list* v 'pad stack))

; Loads

(define/with-types+constants (int long float double array) (0 1 2 3)
  ((load index) stack)
  (push this-type (local-variable index) stack))

; Stores

(define/with-types+constants (int long float double array) (0 1 2 3)
  ((store index) stack)
  (let-values ([(v stack) (pop this-type stack)])
    (set-local-variable! index v)
    stack))

; Stack

(define ((:pop) stack)
  (cdr stack))

(define ((:pop2) stack)
  (cddr stack))

(define ((:dup) stack)
  (cons (car stack) stack))

(define ((:dup_x1) stack)
  (list* (car stack) (cadr stack)
         (car stack)
         (cddr stack)))

(define ((:dup_x2) stack)
  (list* (car stack) (cadr stack) (caddr stack)
         (car stack)
         (cdddr stack)))

(define ((:dup2) stack)
  (list* (car stack) (cadr stack)
         stack))

(define ((:dup2_x1) stack)
  (list* (car stack) (cadr stack) (caddr stack)
         (car stack) (cadr stack)
         (cdddr stack)))

(define ((:dup2_x2) stack)
  (list* (car stack) (cadr stack) (caddr stack) (cadddr stack)
         (car stack) (cadr stack)
         (cddddr stack)))

(define ((:swap) stack)
  (list* (cadr stack) (car stack)
         (cddr stack)))

; Math

; TODO: overflow
(generate/template ([name (add sub mul)]
                    [op (+ - *)])
  (define/with-types (int long float double)
    ((name) stack)
    (let*-values ([(v1 stack) (pop this-type stack)]
                  [(v2 stack) (pop this-type stack)])
      (push this-type (op v1 v2) stack))))

; Conversions

; Comparisons

(generate/template ([name (:ifeq :ifne :iflt :ifge :ifgt :ifle)]
                    [op (= (negate =) < >= > <=)])
  (define ((name offset) stack)
    (let-values ([(v stack) (pop 'int stack)])
      (if (op v 0) ((current-jump) offset stack) stack))))

; Control

(define ((:goto offset) stack)
  ((current-jump) offset stack))

(define ((:return) stack) ((current-return)))

; References

(define ((:getstatic class-name field-name field-type) stack)
  (define jclass ((current-resolve-class) class-name))
  (push field-type (send jclass get-jfield field-name) stack))

(define ((:putstatic class-name field-name field-type) stack)
  (define jclass ((current-resolve-class) class-name))
  (let-values ([(v stack) (pop field-type stack)])
    (send jclass set-jfield! field-name v)
    stack))

(define ((:getfield class-name field-name field-type) stack)
  (let-values ([(ref stack) (pop 'ref stack)])
    (push field-type (send ref get-jfield field-name) stack)))

(define ((:putfield class-name field-name field-type) stack)
  (let*-values ([(v stack) (pop field-type stack)]
                [(ref stack) (pop 'ref stack)])
    (send ref set-jfield! field-name v)
    stack))

(define (((make-invoke kind) class-name method-name method-type) stack)
  (define method ((current-resolve-method) kind class-name method-name method-type))
  (define arg-count (method-arg-count method-type))
  (define-values (args stack-rest)
    (split-at stack (if (eq? 'static kind) arg-count (add1 arg-count))))
  (push (method-return-type method-type)
        (send method invoke . args)
        stack-rest))

(define :invokevirtual (make-invoke 'virtual))
(define :invokespecial (make-invoke 'special))
(define :invokestatic (make-invoke 'static))
(define :invokeinterface (make-invoke 'interface))

(define ((:invokedynamic index z1 z2) stack)
  (error "TODO: invokedynamic"))

(define ((:new name) stack)
  (push 'ref (send ((current-resolve-class) name) new-instance) stack))

(define ((:checkcast type) stack)
  (let ([ref (car stack)])
    (unless (or (eq? 'null ref)
                (send (get-field jclass ref)
                      instance-of?
                      type))
      (error "invalid cast"))
    stack))

(define ((:instanceof type) stack)
  (let-values ([(ref stack) (pop 'ref stack)])
    (push 'int
          (if (and (not (eq? 'null ref))
                   (send (get-field jclass ref)
                         instance-of?
                         type))
              1
              0)
          stack)))
