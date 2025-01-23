#lang racket

(define expr
  (read
   (open-input-string
    "(+ 1 (+ 2 3))")))

(define adder
  (read
   (open-input-string
    "(architecture half-adder ([input a] [input b] [output s] [output co]))
       (assign s  (xor a b))
       (assign co (and a b)))
     ")))