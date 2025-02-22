#lang racket

(require (for-syntax syntax/parse racket/list))

(define-syntax my-let
  (lambda (stx)
    (syntax-parse stx
      [(_ ([v rhs] ...) body)
       (define dup? (check-duplicates
                     (attribute v)
                     (lambda (id1 id2)
                       (bound-identifier=? id1 id2))))
       (when dup?
         (raise-syntax-error #f "duplicate binder" stx dup?))
       #'((lambda (v ...) body) rhs ...)])))

(let ([x 5]
      [x 6])
  x)

(define-syntax weird-let
  (lambda (stx)
    (syntax-parse stx
      [(_ ([v rhs]) body)
       #'(my-let ([v rhs]
                  [x 6])
           body)])))

(weird-let ([x 5])
  x)