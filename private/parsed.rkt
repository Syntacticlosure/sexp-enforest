#lang racket
;; use parsed strucutre to prevent further expansion

(require syntax/parse)
(provide parsed pack-parsed unpack-parsed)

(define (pack-parsed stx)
  (datum->syntax #f `(#:parsed ,stx)))

(define (unpack-parsed stx)
  (syntax-parse stx
    [(#:parsed content)
     #'content]))

(define-syntax-class parsed
  (pattern ((~and (~datum #:parsed) ctx) tail ...)))