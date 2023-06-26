#lang racket
(require (for-syntax syntax/parse racket)
         syntax/parse
         "parsed.rkt")
;; enforest:
(provide define-enforest)
(define (call-operator-as-transformer transformer-ref op stx)
  ;; check whether operator's transformer is bounded in space or global space
  ;; and then use apply the transformer
  (define operator-proc (transformer-ref (syntax-local-value op (λ () #f))))
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
                         #:defaults ([in-space #'values]))
              (~optional (~seq #:transformer-ref transformer-ref)
                         #:defaults ([transformer-ref #'values]))) ...)
     #`(begin
         (define (call-implicit-form op-sym ctx stx)
           (define implicit-id (in-space (datum->syntax ctx op-sym)))
           (define result (call-operator-as-transformer transformer-ref implicit-id #`(#,implicit-id #,@stx)))
           (unless result (raise-syntax-error 'enforest (format "~a transformer is unbound." op-sym) (syntax->datum stx)))
           result)
         (define (enforest stx)
           (syntax-parse stx
             [_:parsed stx]
             [(operator args (... ...))
              (enforest (or (and (identifier? #'operator)
                                 (syntax-local-value (in-space #'operator) (λ () #f))
                                 (or (call-operator-as-transformer transformer-ref (in-space #'operator) stx)
                                     (call-implicit-form '#%literal #'operator #`(#,stx))))
                            (call-implicit-form '#%call #'operator #'(operator args (... ...)))))]
             [operator
              ;; operator is a identifier macro, or just literal
              (enforest (or (and (identifier? #'operator)
                                 (call-operator-as-transformer
                                  transformer-ref
                                  (in-space #'operator) stx))
                            (call-implicit-form '#%literal #'operator #'(operator))))]))
         (define-syntax-class form-class
           (pattern form
             #:with parsed (unpack-parsed (enforest #'form)))))]))
            
