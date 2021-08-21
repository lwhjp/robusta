#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/stx)
         racket/provide
         racket/splicing
         racket/stxparam)

(provide (filtered-out
          (λ (name)
            (and (regexp-match? #rx"^:" name)
                 (substring name 1)))
          (all-defined-out))
         current-locals)

(define current-locals (make-parameter 'current-locals))

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

(define ((:bipush v) stack) (push 'byte v stack))

(define ((:sipush v) stack) (push 'short v stack))

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

; Math

; TODO: overflow
(generate/template ([name (add sub mul)]
                    [op (+ - *)])
  (define/with-types (int long float double)
    ((name) stack)
    (let*-values ([(v1 stack) (pop this-type stack)]
                  [(v2 stack) (pop this-type stack)])
      (push this-type (op v1 v2) stack))))

; Control

(define-syntax-parameter :return #f)

; References

(define ((:invokestatic index) stack)
  (error "TODO: invokestatic"))
