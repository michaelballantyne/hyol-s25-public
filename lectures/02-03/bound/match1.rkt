#lang racket

(require (for-syntax syntax/parse))

(define (f l)
  (match l
    [(list x x)
     'same]
    [(list x y)
     'different]))

(f (list 1 1))
(f (list 1 2))

