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
         "class-file/constant.rkt"
         "jar.rkt"
         "type.rkt"
         "type-descriptor.rkt"
         "vm.rkt")

(provide jclass%
         jfield%
         jmethod%
         class-loader%)

(define (get-constant class-file index)
  (vector-ref (get-field constants class-file) (sub1 index)))

(define (get-value class-file index)
  (get-field value (get-constant class-file index)))

(define (get-value/ldc class-file index loader)
  (define info (get-constant class-file index))
  (cond
    [(field-bound? value info) (get-field value info)]
    [(is-a? info constant:class%)
     (send loader load-class (get-value class-file (get-field name-index info)))]
    [else ; TODO: load method reference
     'null]))

(define (get-class-name class-file index)
  (define class-info (get-constant class-file index))
  (define name (get-value class-file (get-field name-index class-info)))
  (string-replace name "/" "."))

(define (get-type class-file index)
  (define info (get-constant class-file index))
  (if (is-a? info constant:class%)
      `(class . ,(get-value class-file (get-field name-index info)))
      (parse-type-descriptor (get-field value info))))

(define (get-name-and-type class-file index)
  (define info (get-constant class-file index))
  (values (get-value class-file (get-field name-index info))
          (get-type class-file (get-field type-index info))))

(define (resolve-reference class-file index)
  (define ref (get-constant class-file index))
  (define class-name
    (get-class-name class-file (get-field class-index ref)))
  (define-values (method-name method-type)
    (get-name-and-type class-file (get-field name-and-type-index ref)))
  (list class-name method-name method-type))

(define jclass%
  (class object%
    (init-field access-flags
                name
                super
                class-loader)
    (super-new)
    (abstract get-jfield
              get-method
              instance-of?
              new-instance
              set-jfield!)))

(define loaded-class%
  (class jclass%
    (inherit-field class-loader)
    (init class-file)
    (super-new [access-flags (get-field access-flags class-file)]
               [name (datum-intern-literal
                      (get-class-name class-file (get-field this-class class-file)))]
               [super (let ([index (get-field super-class class-file)])
                        (if (zero? index)
                            #f
                            (let ([name (get-class-name class-file index)])
                              (delay (send class-loader load-class name)))))])
    (field [interfaces (for/list ([index (in-vector (get-field interfaces class-file))])
                         (let ([name (get-class-name class-file index)])
                           (delay (send class-loader load-class name))))]
           [fields (for/hash ([info (in-vector (get-field fields class-file))])
                     (let ([jmethod (make-object loaded-method% class-file info this)])
                       (values (get-field name jmethod) jmethod)))]
           [methods (for/hash ([info (in-vector (get-field methods class-file))])
                      (let ([jfield (make-object loaded-method% class-file info this)])
                        (values (get-field name jfield) jfield)))])
    (define field-values (hash)) ; TODO: initialize and check
    (inspect #f)
    (define/override (get-jfield name)
      (hash-ref field-values name))
    (define/override (get-method kind name type)
      ; TODO: check superclass etc (method depends on kind)
      ; TODO: method overloading
      (hash-ref methods name))
    (define/override (instance-of? type)
      ; TODO
      #f)
    (define/override (new-instance)
      (make-object jobject%
        this
        (for/hash ([(name field) (in-hash fields)])
          ; FIXME: parent fields
          ; FIXME: appropriate init value
          (values name 'null))))
    (define/public (resolve)
      (error "TODO: resolve"))
    (define/override (set-jfield! name v)
      (hash-set! field-values name v))))

(define jfield%
  (class object%
    (super-new)
    (init-field access-flags
                name
                type)))

(define loaded-field%
  (class jfield%
    (init class-file
          info
          declaring-class)
    (super-new [access-flags (get-field access-flags info)]
               [name (datum-intern-literal
                      (get-value class-file (get-field name-index info)))]
               [type (parse-type-descriptor
                      (get-value class-file (get-field descriptor-index info)))])
    (define attributes
      (parse-attributes class-file (get-field attributes info)))
    ; TODO: handle attributes
    (inspect #f)))

(define-namespace-anchor here/ns)

(define jit-ns
  (parameterize ([current-namespace (namespace-anchor->empty-namespace here/ns)])
    (namespace-require '(only racket/base #%app #%datum quote))
    (namespace-require 'robusta/private/instructions)
    (current-namespace)))

(define jmethod%
  (class object%
    (init-field access-flags
                name
                type)
    (super-new)
    (abstract invoke)))

(define loaded-method%
  (class jmethod%
    (init class-file
          info)
    (init-field declaring-class)
    (super-new [access-flags (get-field access-flags info)]
               [name (datum-intern-literal
                      (get-value class-file (get-field name-index info)))]
               [type (parse-type-descriptor
                      (get-value class-file (get-field descriptor-index info)))])
    (define attributes
      (parse-attributes class-file (get-field attributes info)))
    ; TODO: handle other attributes
    (define proc
      (cond
        [(hash-ref attributes "Code" #f)
         => (λ (attr)
              (define instructions
                (bytecode->instructions
                 (get-field code attr)
                 #:get-class-name
                 (λ (index) (get-class-name class-file index))
                 #:get-constant
                 (λ (index) (get-value/ldc class-file index (get-field class-loader declaring-class)))
                 #:get-reference
                 (λ (index) (resolve-reference class-file index))
                 #:get-type
                 (λ (index) (get-type class-file index))))
              (define method-stx
                (with-syntax ([(ins ...) (map (λ (p)
                                                (namespace-syntax-introduce
                                                 (datum->syntax #f (cdr p))
                                                 jit-ns))
                                              instructions)])
                #`(λ args
                    (let/ec return
                      (parameterize
                          ([current-jump (λ (offset stack) (error 'TODO))] ; TODO
                           [current-locals (make-vector #,(get-field max-locals attr) 'null)]
                           [current-return return])
                        (for ([a (in-list args)]
                              [i (in-naturals)])
                          (vector-set! (current-locals) i a))
                        (let* ([stack '()]
                               [stack (ins stack)] ...)
                          (error "unexpected end of method")))))))
              (eval-syntax method-stx))]
        [else #f]))
    (inspect #f)
    (define/override (invoke . args)
      (parameterize
          ([current-resolve-class
            (λ (name)
              (define loader (get-field class-loader declaring-class))
              (send loader load-class name))]
           [current-resolve-method
            (λ (kind c m t)
              (define jclass ((current-resolve-class) c))
              (send jclass get-method kind m (method-arg-types t)))])
        (apply proc args)))))

(define (parse-attributes class-file vec)
  (for/hash ([attr (in-vector vec)])
    (values (get-value class-file (get-field attribute-name-index attr))
            attr)))

(define jobject%
  (class object%
    (super-new)
    (init-field jclass
                fields)
    (define/public (get-jfield name)
      (hash-ref fields name))
    (define/public (set-jfield! name v)
      (hash-set! fields name v))))

(define (port->jclass loader name [in (current-input-port)])
  (define c (new loaded-class%
                 [class-file (read-class-file in)]
                 [class-loader loader]))
  (unless (string=? name (get-field name c))
    (error "class file name mismatch"))
  c)

(define class-loader%
  (class object%
    (init-field [classpath '()])
    (init [bootstrap-classes '()])
    (super-new)
    (define loaded-classes
      (make-hash (map (λ (c) (cons (get-field name c) c))
                      bootstrap-classes)))
    (define (call-with-class-file name proc)
      (let/ec return
        (for ([path (in-list classpath)])
          (cond
            [(and (string-suffix? path ".jar") (file-exists? path))
             (let* ([jar (get-jar path)]
                    [class-file (send jar get-class-file name)])
               (when class-file
                 (return (call-with-input-bytes class-file proc))))]
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
       this
       name
       (cond
         [(input-port? in) in]
         [else (open-input-bytes in)])))
    (define jar-cache (make-hash))
    (define/public (get-jar path)
      (hash-ref! jar-cache path (λ () (make-object jar% path))))
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
