#lang racket

;; provide utilities for binding spaces

(provide make-binding-space)

(define (make-binding-space sym)
  (define introducer (make-interned-syntax-introducer sym))
  (λ (stx)
    (introducer stx 'add)))
