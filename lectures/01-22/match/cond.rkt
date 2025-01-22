#lang racket

(define (f x)
  (cond
    [(= x 1) 'a]
    [(= x 2) 'b]
    [else 'c]))

