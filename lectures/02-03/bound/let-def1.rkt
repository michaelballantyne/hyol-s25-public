#lang racket

(require (for-syntax syntax/parse))

(define-syntax my-let
  (lambda (stx)
    (syntax-parse stx
      [(_ ([v rhs] ...) body)
       (define dup? (check-duplicate-identifier (attribute v)))
       (when dup?
         (raise-syntax-error #f "duplicate binding" stx dup?))
       #'((lambda (v ...) body) rhs ...)])))

(my-let ([x 5]
         [x 6])
  x)

