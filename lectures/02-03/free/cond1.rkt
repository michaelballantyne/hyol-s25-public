#lang racket

(require rackunit)

(define else #f)

(define (length x)
  (cond
    [(empty? x) 0]
    [else (+ 1 (length (rest x)))]))

(check-equal?
 (length '(a b c))
 3)