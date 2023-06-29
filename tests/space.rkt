#lang racket
(require rackunit "../main.rkt"
         (for-syntax syntax/parse))

(define-binding-space sexp-enforest/test
  #:in-space in-test-space
  #:syntax-definer define-test-syntax)

(define-syntax (run-test stx)
  (syntax-parse stx
    [(_ e) (in-test-space #'e)]))

(define-test-syntax (test1 stx)
  #`1)

(define-test-syntax test2
  (λ (stx)
    #`2))

(module+ test
  (check-equal? (run-test test1) 1)
  (check-equal? (run-test test2) 2))
