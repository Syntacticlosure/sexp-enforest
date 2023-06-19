#lang racket
(require (for-syntax syntax/parse )
         syntax/parse
         "parsed.rkt")
;; enforest:
(provide define-enforest)
(define (call-operator-as-transformer op stx)
  ;; check whether operator's transformer is bounded in space or global space
  ;; and then use apply the transformer
  (define operator-proc (syntax-local-value op (λ () #f)))
  (and operator-proc
       (syntax-local-apply-transformer operator-proc
                                       op
                                       'expression
                                       #f
                                       stx)))

(define-syntax (define-enforest stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:enforest enforest)
                         #:defaults ([enforest #'enforest]))
              (~optional (~seq #:form-class form-class)
                         #:defaults ([form-class #'form-class]))
              (~optional (~seq #:in-space in-space)
                         #:defaults ([in-space #'in-space]))) ...)
     #`(begin
         (define (enforest stx)
           (syntax-parse stx
             [_:parsed stx]
             [(operator:id args (... ...))
              #:with implicit-call-form (in-space (datum->syntax #'operator '#%call))
              (enforest (or (call-operator-as-transformer (in-space #'operator) stx) #'(implicit-call-form operator args (... ...))))]
             [(operator args (... ...))
              ;; call form case
              #:with implicit-call-form (in-space (datum->syntax #'operator '#%call))
              (enforest #'(implicit-call-form operator args (... ...)))]
             [operator:id
              ;; operator is a identifier macro, or just literal
              #:with implicit-literal-form (in-space (datum->syntax #'operator '#%literal))
              (enforest (or (call-operator-as-transformer
                             (in-space #'operator) stx)
                            #'(implicit-literal-form operator)))]
             [lit
              ;; literal form case
              #:with implicit-literal-form (in-space (datum->syntax #'lit '#%literal))
              (enforest #'(implicit-literal-form lit))]))
         (define-syntax-class form-class
           (pattern form
             #:with parsed (unpack-parsed (enforest #'form)))))]))
            

                 
     

