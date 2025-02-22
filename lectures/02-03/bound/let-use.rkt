#lang racket

(require (for-syntax syntax/parse))

#;(let ([x 5]
      [x 6])
  x)

(define-syntax weird-let
  (lambda (stx)
    (syntax-parse stx
      [(_ ([v rhs]) body)
       #'(let ([v rhs]
               [x 6])
           body)])))

(weird-let ([x 5])
  x)






#;(let ([x 5]
      [x 6])
  x)