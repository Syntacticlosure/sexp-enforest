#lang racket
;; utilities for structy properties

(require (for-syntax syntax/parse racket/syntax))
(provide property)
(define-syntax (property stx)
  (syntax-parse stx
    [(_ prop:id accessor:id)
     #:with prop-descriptor (format-id #'prop "prop:~a" #'prop)
     #:with prop-v (format-id #'prop "~a-v" #'prop)
     #`(begin
         (define-values (prop-descriptor _  prop-ref)
           (make-struct-type-property 'prop))
         (define (accessor v)
           (define acc (prop-ref v #f))
           (and acc (acc v)))
         (struct prop (v) #:transparent
           #:property prop-descriptor (λ (x) (prop-v x))))]))