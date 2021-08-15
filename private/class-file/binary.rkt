#lang racket/base

(require racket/match
         binary-class
         "../modified-utf8.rkt")

(provide (all-defined-out))

(define (array length element-type)
  (binary
   (λ (in)
     (for/vector #:length length
                 ([i (in-range length)])
       (read-value element-type in)))
   (λ (out v)
     (unless (and (vector? v) (= length (vector-length v)))
       (raise-argument-error 'array
                             (format "vector of length ~a" length)
                             v))
     (for ([e (in-vector v)])
       (write-value element-type e)))))

(define (length-prefixed-array length-type element-type [length-offset 0])
  (binary
   (λ (in)
     (define length (- (read-value length-type in) length-offset))
     (read-value (array length element-type) in))
   (λ (out v)
     (unless (vector? v)
       (raise-argument-error 'length-prefixed-array "vector?" v))
     (define length (vector-length v))
     (write-value length-type out (+ length length-offset))
     (write-value (array length element-type) out v))))

(define (length-prefixed-bytestring length-type)
  (binary
   (λ (in)
     (define length (read-value length-type in))
     (read-bytes length in))
   (λ (out v)
     (unless (bytes? v)
       (raise-argument-error 'length-prefixed-bytestring "bytes?" v))
     (write-value length-type out (bytes-length v))
     (write-bytes v out))))

(define (length-prefixed-modified-utf8-string length-type)
  (binary
   (λ (in)
     (bytes->string/modified-utf-8
      (read-value (length-prefixed-bytestring length-type) in)))
   (λ (out v)
     (write-value (length-prefixed-bytestring length-type) out
      (string->bytes/modified-utf-8 v)))))

(define (verify-length-prefixed length-type . types)
  (binary
   (λ (in)
     (define expected-length (read-value length-type in))
     (define start-pos (file-position in))
     (define vs (map (λ (t) (read-value t in)) types))
     (define end-pos (file-position in))
     (define actual-length (- end-pos start-pos))
     (unless (= expected-length actual-length)
       (error 'verify-length
              "expected length: ~a\nactual length: ~a\n"
              expected-length
              actual-length))
     (apply values vs))
   (λ (out . vs)
     (define length-pos (file-position out))
     (write-value length-type out 0)
     (define start-pos (file-position out))
     (for ([v (in-list vs)]
           [t (in-list types)])
       (write-value t out v))
     (define end-pos (file-position out))
     (define length (- end-pos start-pos))
     (write-value (ref length-pos length-type length)))))

(define (flags base-type id-spec)
  (define ids
    (let loop ([id-spec id-spec]
               [next-bit 1])
      (match id-spec
        ['() '()]
        [(cons `[,id ,bit] rest)
         (cons (cons id bit)
               (loop rest (arithmetic-shift bit 1)))]
        [(cons id rest)
         (cons (cons id next-bit)
               (loop rest (arithmetic-shift next-bit 1)))])))
  ; FIXME: ignores unknown bits/flags
  (binary
   (λ (in)
     (define v (read-value base-type in))
     (for/list ([flag (in-list ids)]
                #:when (= (cdr flag) (bitwise-and v (cdr flag))))
       (car flag)))
   (λ (out v)
     (unless (list? v)
       (raise-argument-error 'flags "list?" v))
     (write-value base-type out (for/fold ([a 0])
                                          ([id (in-list v)])
                                  (cdr (assoc id ids)))))))
