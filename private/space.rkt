#lang racket

;; provide utilities for binding spaces
(require (for-syntax syntax/parse))
(provide (for-syntax make-binding-space) define-binding-space)


(begin-for-syntax 
  (define (make-binding-space sym)
    (define introducer (make-interned-syntax-introducer sym))
    (λ (stx)
      (introducer stx 'add))))


(define-syntax (define-binding-space stx)
  (syntax-parse stx
    [(_ space-path:id (~alt (~optional (~seq #:in-space in-space))
                            (~optional (~seq #:syntax-definer syntax-definer))) ...)
     #`(begin
         (begin-for-syntax (~? (define in-space (make-binding-space 'space-path))))
         (define-syntax (syntax-definer stx)
           (syntax-parse stx
             [(_ name:id body:expr) #`(define-syntax #,(in-space #'name)
                                        body)]
             [(_ (name:id . args) bodies (... ...)) #`(define-syntax (#,(in-space #'name) . args)
                                                        bodies (... ...))])))]))
