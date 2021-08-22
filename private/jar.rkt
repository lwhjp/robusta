#lang racket

(require racket/class
         file/unzip)

(provide jar%)

(define (parse-manifest path)
  (define lines (call-with-input-file path port->lines))
  (define pairs
    (filter-map (λ (line)
                  (cond
                    [(regexp-match #rx"^(.*?): *(.*)" line)
                     => (λ (m) (cons (cadr m) (caddr m)))]
                    [else #f]))
                lines))
  (define (not-name p) (not (equal? "Name" (car p))))
  (define-values (main-pairs entry-pairs) (splitf-at pairs not-name))
  (define entries
    (let loop ([pairs entry-pairs])
      (if (null? pairs)
          '()
          (let-values ([(this-entry rest) (splitf-at (cdr pairs) not-name)])
            (cons (cons (car pairs) this-entry)
                  (loop rest))))))
  (values main-pairs entries))

(define jar%
  (class object%
    (super-new)
    (init-field path)
    (define-values (main-attributes entry-attributes)
      (with-handlers ([exn:fail:unzip:no-such-entry? (λ (exn) (values '() '()))])
        (call-with-unzip-entry path "META-INF/MANIFEST.MF" parse-manifest)))
    (init-field [main-class (cond
                              [(assoc "Main-Class" main-attributes) => cdr]
                              [else #f])])
    (define/public (get-class-file name)
      (with-handlers ([exn:fail:unzip:no-such-entry? (λ (exn) #f)])
        (call-with-unzip-entry
         path
         (string-append (string-replace name "." "/") ".class")
         (λ (path) (call-with-input-file path port->bytes)))))))
