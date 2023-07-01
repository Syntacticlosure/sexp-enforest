#lang racket
(require "../main.rkt" rackunit)

(property test test-ref)

(module+ test
  (check-equal? (test-ref 1) #f)
  (check-equal? (test-ref (test 1)) 1))