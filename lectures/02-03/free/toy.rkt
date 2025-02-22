#lang racket

(require (for-syntax syntax/parse))

(define x 5)

(define-syntax is-x
  (lambda (stx)
    (syntax-parse stx
      [(_ v:id)
       (if (free-identifier=? #'v #'x)
           #'#t
           #'#f)])))

(let ([x 5])
  (is-x x))