#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/match)

(provide jchar-vector->string
         bytes->jchar-vector/modified-utf-8
         bytes->string/modified-utf-8
         string->jchar-vector
         jchar-vector->bytes/modified-utf-8
         string->bytes/modified-utf-8)

(module+ test
  (require rackunit))

; FIXME: catch invalid encodings (encoded surrogates; unnecessary multibyte encodings etc)

(define-match-expander integer-in
  (syntax-parser
    [(_ from:integer to:integer pat ...)
     #'(? (λ (v) (<= from v to)) pat ...)]))

(define (jchar-vector->string vec)
  (list->string
   (map integer->char
        (let loop ([cs (vector->list vec)])
          (match cs
            ['() '()]
            [(list-rest (or (integer-in #x0000 #xD7FF c)
                            (integer-in #xE000 #xFFFF c))
                        rest)
             (cons c (loop rest))]
            [(list-rest (integer-in #xD800 #xDBFF c1)
                        (integer-in #xDC00 #xDFFF c2)
                        rest)
             (cons (+ #x10000
                      (bitwise-ior (arithmetic-shift (bitwise-and #x3FF c1) 10)
                                   (bitwise-and #x3FF c2)))
                   (loop rest))]
            [_ (error 'jchar-vector->string "invalid encoding")])))))

(module+ test
  (check-equal? (jchar-vector->string #(#x0024)) "\u0024")
  (check-equal? (jchar-vector->string #(#x00A2)) "\u00A2")
  (check-equal? (jchar-vector->string #(#x0939)) "\u0939")
  (check-equal? (jchar-vector->string #(#x20AC)) "\u20AC")
  (check-equal? (jchar-vector->string #(#xD55C)) "\uD55C")
  (check-equal? (jchar-vector->string #(#xD800 #xDF48)) "\U10348"))

(define (bytes->jchar-vector/modified-utf-8 bstr)
  ; modified UTF-8 encodes UTF-16 surrogate pairs, so we can't use bytes->string/utf-8
  (list->vector
   (let loop ([bs (bytes->list bstr)])
     (match bs
       ['() '()]
       [(list-rest (integer-in #x01 #x7F c) rest)
        (cons c (loop rest))]
       [(list-rest (integer-in #xC0 #xDF c1)
                   (integer-in #x80 #xBF c2)
                   rest)
        (cons (bitwise-ior (arithmetic-shift (bitwise-and #x1F c1) 6)
                           (bitwise-and #x3F c2))
              (loop rest))]
       [(list-rest (integer-in #xE0 #xEF c1)
                   (integer-in #x80 #xBF c2)
                   (integer-in #x80 #xBF c3)
                   rest)
        (cons (bitwise-ior (arithmetic-shift (bitwise-and #x0F c1) 12)
                           (arithmetic-shift (bitwise-and #x3F c2) 6)
                           (bitwise-and #x3F c3))
              (loop rest))]
       [(list-rest (integer-in #xF0 #xF7 c1)
                   (integer-in #x80 #xBF c2)
                   (integer-in #x80 #xBF c3)
                   (integer-in #x80 #xBF c4)
                   rest)
        (cons (bitwise-ior (arithmetic-shift (bitwise-and #x07 c1) 18)
                           (arithmetic-shift (bitwise-and #x3F c2) 12)
                           (arithmetic-shift (bitwise-and #x3F c3) 6)
                           (bitwise-and #x3F c4))
              (loop rest))]
       [_ (error 'bytes->jchar-vector/modified-utf-8 "invalid encoding")]))))

(module+ test
  (check-equal? (bytes->jchar-vector/modified-utf-8 (bytes #xC0 #x80)) #(#x0000))
  (check-equal? (bytes->jchar-vector/modified-utf-8 (bytes #x24)) #(#x0024))
  (check-equal? (bytes->jchar-vector/modified-utf-8 (bytes #xC2 #xA2)) #(#x00A2))
  (check-equal? (bytes->jchar-vector/modified-utf-8 (bytes #xE0 #xA4 #xB9)) #(#x0939))
  (check-equal? (bytes->jchar-vector/modified-utf-8 (bytes #xE2 #x82 #xAC)) #(#x20AC))
  (check-equal? (bytes->jchar-vector/modified-utf-8 (bytes #xED #x95 #x9C)) #(#xD55C))
  (check-equal? (bytes->jchar-vector/modified-utf-8 (bytes #xED #xA1 #x80 #xED #xBD #x88)) #(#xD840 #xDF48)))

(define (bytes->string/modified-utf-8 bstr)
  (jchar-vector->string
   (bytes->jchar-vector/modified-utf-8 bstr)))

(module+ test
  (check-equal? (bytes->string/modified-utf-8 (bytes)) "")
  (check-equal? (bytes->string/modified-utf-8 (bytes #xC0 #x80)) "\u0000")
  (check-equal? (bytes->string/modified-utf-8 (bytes #xED #xA0 #xB4 #xED #xB4 #x9E)) "\U1D11E")
  (check-equal? (bytes->string/modified-utf-8 #"hello") "hello")
  (check-equal? (bytes->string/modified-utf-8
                 (bytes #xE3 #x81 #x93 #xE3 #x82 #x93 #xE3 #x81 #xAB #xE3 #x81 #xA1 #xE3 #x81 #xAF))
                "こんにちは")
  (check-equal? (bytes->string/modified-utf-8
                 (bytes #x67 #x6C #xED #xA0 #xBC #xED #xBC #x90 #x62 #x65))
                "gl\U0001F310be"))

(define (string->jchar-vector str)
  (list->vector
   (let loop ([chars (map char->integer (string->list str))])
     (match chars
       ['() '()]
       [(cons c rest)
        (cond
          [(< c #x10000) (cons c (loop rest))]
          [(<= c #x10FFFF)
           (let* ([c^ (- c #x10000)]
                  [hi-bits (arithmetic-shift c^ -10)]
                  [lo-bits (bitwise-and #x3FF c^)])
             (list* (bitwise-ior #xD800 hi-bits)
                    (bitwise-ior #xDC00 lo-bits)
                    (loop rest)))]
          [else (error 'string->char-vector "invalid codepoint")])]))))

(module+ test
  (check-equal? (string->jchar-vector "\u0024") #(#x0024))
  (check-equal? (string->jchar-vector "\u00A2") #(#x00A2))
  (check-equal? (string->jchar-vector "\u0939") #(#x0939))
  (check-equal? (string->jchar-vector "\u20AC") #(#x20AC))
  (check-equal? (string->jchar-vector "\uD55C") #(#xD55C))
  (check-equal? (string->jchar-vector "\U10348") #(#xD800 #xDF48)))

(define (jchar-vector->bytes/modified-utf-8 vec)
  (list->bytes
   (let loop ([chars (vector->list vec)])
     (match chars
       ['() '()]
       [(cons c rest)
        (cond
          [(= c #x0000)
           (list* #xC0 #x80 (loop rest))]
          [(<= c #x007F)
           (list* c (loop rest))]
          [(<= c #x07FF)
           (list* (bitwise-ior #xC0 (arithmetic-shift c -6))
                  (bitwise-ior #x80 (bitwise-and #x3F c))
                  (loop rest))]
          [(<= c #xFFFF)
           (list* (bitwise-ior #xE0 (arithmetic-shift c -12))
                  (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -6)))
                  (bitwise-ior #x80 (bitwise-and #x3F c))
                  (loop rest))]
          [(<= c #x10FFFF)
           (list* (bitwise-ior #xF0 (arithmetic-shift c -18))
                  (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -12)))
                  (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -6)))
                  (bitwise-ior #x80 (bitwise-and #x3F c))
                  (loop rest))]
          [else (error 'jchar-vector->bytes/modified-utf-8 "invalid character value")])]))))

(module+ test
  (check-equal? (jchar-vector->bytes/modified-utf-8 #(#x0000)) (bytes #xC0 #x80))
  (check-equal? (jchar-vector->bytes/modified-utf-8 #(#x0024)) (bytes #x24))
  (check-equal? (jchar-vector->bytes/modified-utf-8 #(#x00A2)) (bytes #xC2 #xA2))
  (check-equal? (jchar-vector->bytes/modified-utf-8 #(#x0939)) (bytes #xE0 #xA4 #xB9))
  (check-equal? (jchar-vector->bytes/modified-utf-8 #(#x20AC)) (bytes #xE2 #x82 #xAC))
  (check-equal? (jchar-vector->bytes/modified-utf-8 #(#xD55C)) (bytes #xED #x95 #x9C))
  (check-equal? (jchar-vector->bytes/modified-utf-8 #(#xD840 #xDF48)) (bytes #xED #xA1 #x80 #xED #xBD #x88)))

(define (string->bytes/modified-utf-8 str)
  (jchar-vector->bytes/modified-utf-8
   (string->jchar-vector str)))

(module+ test
  (check-equal? (string->bytes/modified-utf-8 "") (bytes))
  (check-equal? (string->bytes/modified-utf-8 "\u0000") (bytes #xC0 #x80))
  (check-equal? (string->bytes/modified-utf-8 "\U1D11E") (bytes #xED #xA0 #xB4 #xED #xB4 #x9E))
  (check-equal? (string->bytes/modified-utf-8 "hello") #"hello")
  (check-equal? (string->bytes/modified-utf-8 "こんにちは")
                (bytes #xE3 #x81 #x93 #xE3 #x82 #x93 #xE3 #x81 #xAB #xE3 #x81 #xA1 #xE3 #x81 #xAF))
  (check-equal? (string->bytes/modified-utf-8 "gl\U0001F310be")
                (bytes #x67 #x6C #xED #xA0 #xBC #xED #xBC #x90 #x62 #x65)))
