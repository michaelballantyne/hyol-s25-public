#lang racket

(require (for-syntax syntax/parse))

(define (f l)
  (match l
    [(list x x)
     'same]
    [(list x y)
     'different]))

#;(f (list 1 1))
#;(f (list 1 2))




(define-syntax weird-match
  (lambda (stx)
    (syntax-parse stx
      [(_ v l)
       #'(match l
           [(list v x)
            'matches])])))

(weird-match x (list 1 2))

(match (list 1 2)
  [(list x_u x_d)
   'matches])



