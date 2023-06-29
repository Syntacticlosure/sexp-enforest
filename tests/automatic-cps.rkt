#lang racket
(require "../main.rkt" (for-syntax syntax/parse)
         rackunit)

(begin-for-syntax
  (define in-cps-space (make-binding-space 'test/cps))
  (define-enforest #:form-class cps-form
    #:in-space in-cps-space))

(define-syntax (define-cps-expander stx)
  (syntax-parse stx
    [(_ (name:id . args) bodies ...)
     #`(define-syntax (#,(in-cps-space #'name) . args) bodies ...)]))


(define-for-syntax (evaluate-cps stx-list var-list body)
  (if (null? stx-list)
      body
      (syntax-parse (car stx-list)
        [p:cps-form #`(p.parsed (λ (#,(car var-list))
                                  #,(evaluate-cps (cdr stx-list)
                                                  (cdr var-list) body)))])))
                      
(define-cps-expander (#%call stx)
  (syntax-parse stx
    [(_ f args ...)
     #:with (f- args- ...) (generate-temporaries #'(f args ...))
     (pack-parsed
      #`(λ (k)
          #,(evaluate-cps (syntax-e #'(f args ...)) (syntax-e #'(f- args- ...)) #'(k (f- args- ...)))))]))

(define-cps-expander (#%literal stx)
  (syntax-parse stx
    [(_ lit)
     (pack-parsed #`(λ (k) (k lit)))]))

(define-cps-expander (#%reset stx)
  (syntax-parse stx
    [(_ expr:cps-form)
     (pack-parsed
      #` (λ (k)
           (k (expr.parsed (λ (x) x)))))]))

(define-cps-expander (#%shift stx)
  (syntax-parse stx
    [(_ k:id expr:cps-form)
     
     (pack-parsed
      #` (λ (c)
           (let ([k (λ (v) (c v))])
             (expr.parsed (λ (v) v)))))]))


(define-syntax (cps stx)
  (syntax-parse stx
    [(_ expr:cps-form)
     #'(expr.parsed (λ (x) x))]))

(module+ test
  (check-equal? (cps 1) 1)
  (check-equal? (cps (+ 1 2)) 3)
  (check-equal? (cps (+ 1 (* 2 3))) 7))

(module+ test
  (check-equal? (cps (+ 1 (#%shift k (list
                                      (k 1) (k 2))))) '(2 3))
  (check-equal? (cps (+ 1 (#%shift k1 (+ 1 (#%shift k2 (k2 1)))))) 2))



#;(cps (+ (let ([x 2]) (+ x 1)) 3))
;; need cps-transformer struct property to treat racket primitive macros as literals
;; or just define them in corresponding binding spaces


(module+ test
  (define-cps-expander (let stx)
    (syntax-parse stx
      [(_ ([var expr:cps-form]) body:cps-form)
       (pack-parsed
        #`(λ (k) (expr.parsed (λ (var)
                                (body.parsed k)))))]))
  ;; now, it works properly
  (check-equal? (cps (+ (let ([x 2]) (+ x 1)) 3)) 6)
  ;; and will not conflict with the origin let
  (check-equal? (let ([x 1][y 2]) (+ x y)) 3))
     



