#lang racket

(define (f x)
  (case/checked x
    [(a) 1]
    [(a b) 2]))

(f 'b)

