#lang racket/base

(require racket/contract/base
         "opcodes.rkt"
         "type.rkt")

(provide
 (contract-out
  [bytecode->instructions
   (->* (bytes?
         #:get-class-name (-> exact-nonnegative-integer? string?)
         #:get-constant (-> exact-nonnegative-integer? any/c)
         #:get-reference (-> exact-nonnegative-integer? (list/c string? string? type?))
         #:get-type (-> exact-nonnegative-integer? type?))
        (exact-nonnegative-integer? exact-nonnegative-integer?)
        (listof (cons/c exact-nonnegative-integer? list?)))]))

(define (bytecode->instructions bstr [start 0] [end (bytes-length bstr)]
                                #:get-class-name get-class-name
                                #:get-constant get-constant
                                #:get-reference get-reference
                                #:get-type get-type)
  (define (get-arg type start)
    (define width
      (case type
        [(u8 s8 0) 1]
        [(u16 s16) 2]
        [(u32 s32) 4]))
    (define signed? (memq type '(s8 s16 s32)))
    (define arg-end (+ start width))
    (unless (<= arg-end end)
      (error 'bytecode->instruction "unexpected end of bytecode"))
    (define arg (integer-bytes->integer bstr signed? #t start arg-end))
    (unless (or (not (equal? '0 type)) (zero? arg))
      (error 'bytecode->instruction "expected zero byte; got: ~e" arg))
    (values arg arg-end))
  (define (lookup-instruction opcode)
    (define spec
      (or (vector-ref opcodes opcode)
          (error 'bytecode->instructions "invalid opcode: ~a" opcode)))
    (values (car spec) (cdr spec)))
  (define (pad4 start)
    (for/fold ([start start])
              ([i (in-range (modulo (- start) 4))])
      (define-values (arg arg-end) (get-arg '0 start))
      arg-end))
  (define (get-args count type start)
    (for/fold ([args '()]
               [start start]
               #:result (values (reverse args) start))
              ([i (in-range count)])
      (define-values (arg arg-end) (get-arg type start))
      (values (cons arg args) arg-end)))
  (let next-instruction ([start start])
    (cond
      [(>= start end) '()]
      [else
       (define opcode (bytes-ref bstr start))
       (define-values (instruction params) (lookup-instruction opcode))
       (define-values (arguments next-start)
         (case instruction
           [(lookupswitch)
            (let*-values ([(arg-end) (pad4 (add1 start))]
                          [(default arg-end) (get-arg 's32 arg-end)]
                          [(npairs arg-end) (get-arg 's32 arg-end)]
                          [(rest arg-end) (get-args (* 2 npairs) 's32 arg-end)])
              (values (list* default npairs rest) arg-end))]
           [(tableswitch)
            (let*-values ([(arg-end) (pad4 (add1 start))]
                          [(default arg-end) (get-arg 's32 arg-end)]
                          [(low arg-end) (get-arg 's32 arg-end)]
                          [(high arg-end) (get-arg 's32 arg-end)]
                          [(offsets arg-end) (get-args 's32 (add1 (- high low)))])
              (values (list* default low high offsets) arg-end))]
           [(wide)
            (let*-values ([(sub-op arg-end) (get-arg 'u8 (add1 start))]
                          [(index arg-end) (get-arg 'u16 arg-end)]
                          [(sub-ins sub-params) (lookup-instruction sub-op)])
              (case sub-ins
                [(iinc)
                 (let-values ([(const arg-end) (get-arg 's16 arg-end)])
                   (values (list sub-ins index const) arg-end))]
                [else (values (list sub-ins index) arg-end)]))]
           [else
            (let next-arg ([params params]
                           [args '()]
                           [start (add1 start)])
              (if (null? params)
                  (values (reverse args) start)
                  (let-values ([(arg arg-end) (get-arg (car params) start)])
                    (next-arg (cdr params) (cons arg args) arg-end))))]))
       (define resolved-arguments
         (case instruction
           [(ldc ldc_w ldc2_w) (list `',(get-constant (car arguments)))]
           [(getstatic putstatic getfield putfield
             invokevirtual invokespecial invokestatic invokeinterface)
            ; Extra args are ignored for invokeinterface
            (map (λ (v) `',v) (get-reference (car arguments)))]
           [(new) (list `',(get-class-name (car arguments)))]
           [(checkcast instanceof) (list `',(get-type (car arguments)))]
           [else arguments]))
       (cons (cons start (cons instruction resolved-arguments))
             (next-instruction next-start))])))

(module+ test
  (require rackunit)
  (check-equal? (bytecode->instructions #"\20\6<\20\a=\e\34h>\35\270\0\2\261"
                                        #:get-class-name list
                                        #:get-constant list
                                        #:get-reference list
                                        #:get-type list)
                '((0 . (bipush 6))
                  (2 . (istore_1))
                  (3 . (bipush 7))
                  (5 . (istore_2))
                  (6 . (iload_1))
                  (7 . (iload_2))
                  (8 . (imul))
                  (9 . (istore_3))
                  (10 . (iload_3))
                  (11 . (invokestatic '2))
                  (14 . (return))))
  (check-equal? (bytecode->instructions #"\e\253\0\0\0\0\0\37\0\0\0\2\0\0\0\0\0\0\0\e\0\0\0\1\0\0\0\35\3\254\4\254\5\254"
                                        #:get-class-name list
                                        #:get-constant list
                                        #:get-reference list
                                        #:get-type list)
                '((0 . (iload_1))
                  (1 . (lookupswitch 31 2 0 27 1 29))
                  (28 . (iconst_0))
                  (29 . (ireturn))
                  (30 . (iconst_1))
                  (31 . (ireturn))
                  (32 . (iconst_2))
                  (33 . (ireturn)))))
