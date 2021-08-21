#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/match
         racket/port
         racket/promise
         racket/string
         racket/stxparam
         "bytecode.rkt"
         "class-file/class-file.rkt"
         (only-in "instructions.rkt"
                  current-locals
                  return)
         "type-descriptor.rkt")

(provide jclass%
         class-loader%)

(define (get-constant class-file index)
  (vector-ref (get-field constants class-file) (sub1 index)))

(define (get-value class-file index)
  (get-field value (get-constant class-file index)))

(define (get-class-name class-file index)
  (define class-info (get-constant class-file index))
  (define name (get-value class-file (get-field name-index class-info)))
  (string-replace name "/" "."))

(define jclass%
  (class object%
    (super-new)
    (init class-file)
    (field [access-flags (get-field access-flags class-file)]
           [name (datum-intern-literal
                  (get-class-name class-file (get-field this-class class-file)))]
           [super (let ([index (get-field super-class class-file)])
                    (if (zero? index)
                        #f
                        (let ([name (get-class-name class-file index)])
                          (delay (load-class name)))))]
           [interfaces (for/list ([index (in-vector (get-field interfaces class-file))])
                         (let ([name (get-class-name class-file index)])
                           (delay (load-class name))))]
           [fields (for/hash ([info (in-vector (get-field fields class-file))])
                     (let ([name (datum-intern-literal
                                  (get-value class-file (get-field name-index info)))])
                       (values name
                               (make-object field%
                                 class-file
                                 this
                                 (get-field access-flags info)
                                 name
                                 (parse-type-descriptor
                                  (get-value class-file (get-field descriptor-index info))
                                  (λ (name) (delay (load-class name))))
                                 (parse-attributes class-file (get-field attributes info))))))]
           [methods (for/hash ([info (in-vector (get-field methods class-file))])
                      (let ([name (datum-intern-literal
                                   (get-value class-file (get-field name-index info)))])
                        (values name
                                (make-object method%
                                  class-file
                                  this
                                  (get-field access-flags info)
                                  name
                                  (parse-type-descriptor
                                   (get-value class-file (get-field descriptor-index info))
                                   (λ (name) (delay (load-class name))))
                                  (parse-attributes class-file (get-field attributes info))))))])
    (inspect #f)
    (define/public (invoke-static-method name)
      (send (hash-ref methods name) invoke))
    (define/public (load-class name)
      (error "TODO: load class:" name))
    (define/public (resolve)
      (error "TODO: resolve"))))

(define field%
  (class object%
    (super-new)
    (init class-file
          declaring-class)
    (init-field access-flags
                name
                type)
    (init attributes)
    ; TODO: handle attributes
    (inspect #f)))

(define-namespace-anchor here/ns)

(define jit-ns
  (parameterize ([current-namespace (namespace-anchor->empty-namespace here/ns)])
    (namespace-require 'robusta/private/instructions)
    (current-namespace)))

(define method%
  (class object%
    (super-new)
    (init class-file
          declaring-class)
    (init-field access-flags
                name
                type)
    (init attributes)
    ; TODO: handle other attributes
    (define proc
      (cond
        [(hash-ref attributes "Code" #f)
         => (λ (attr)
              (define instructions
                (bytecode->instructions (get-field code attr)))
              ; TODO: goto, args, references etc
              (define method-stx
                (with-syntax ([(ins ...) (map cdr instructions)])
                #`(λ ()
                    (let/ec exit
                      (parameterize ([current-locals (make-vector #,(get-field max-locals attr) 'null)])
                        (syntax-parameterize ([return (make-rename-transformer #'exit)])
                          (let* ([stack '()]
                                 [stack (ins stack)] ...)
                            (error "unexpected end of method"))))))))
              (eval method-stx jit-ns))]
        [else #f]))
    (inspect #f)
    (define/public (invoke) (proc))))

(define (parse-attributes class-file vec)
  (for/hash ([attr (in-vector vec)])
    (values (get-value class-file (get-field attribute-name-index attr))
            attr)))

(define (port->jclass name [in (current-input-port)])
  (define c (make-object jclass% (read-class-file in)))
  (unless (string=? name (get-field name c))
    (error "class file name mismatch"))
  c)

(define class-loader%
  (class object%
    (init-field [classpath '()]
                [bootstrap-classes '()])
    (super-new)
    (define loaded-classes (make-hash bootstrap-classes))
    (define (call-with-class-file name proc)
      (let/ec return
        (for ([path (in-list classpath)])
          (cond
            [(and (string-suffix? path ".jar") (file-exists? path))
             (error "TODO: jar")]
            [(directory-exists? path)
             (let ([class-file-path (path-add-extension
                                     (apply build-path path (string-split name "."))
                                     ".class")])
               (when (file-exists? class-file-path)
                 (return (call-with-input-file class-file-path proc))))]
            [else (error "invalid entry in classpath:" path)]))
        (error "class not found:" name)))
    (define/public (define-class name in)
      (port->jclass
       name
       (cond
         [(input-port? in) in]
         [else (open-input-bytes in)])))
    (define/public (load-class name [resolve? #f])
      (define c
        (hash-ref!
         loaded-classes
         name
         (λ ()
           (call-with-class-file name
             (λ (in) (define-class name in))))))
      (when resolve? (resolve-class c))
      c)
    (define/public (resolve-class cls)
      (error 'todo))))
