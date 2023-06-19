#lang racket
(require (for-syntax "../private/parsed.rkt"))
(require (for-syntax syntax/parse) rackunit)

(module+ test
  (define-syntax (test stx)
    (syntax-parse (pack-parsed #'1)
      [p:parsed #'#t]
      [_ #'#f]))
  (check-equal? (test) #t))


