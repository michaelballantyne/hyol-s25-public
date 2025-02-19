#lang racket

(require "peg-embedded.rkt" rackunit)


(define-peg digit (char char-numeric?))

(define-peg num (alt (seq digit num)
                     digit))

(define op (alt (char #\+)
                (char #\-)))

(define-peg arith-expr (seq num op num))


(check-equal?
 (peg-parse arith-expr "1+23")
 "1+23")








(check-equal?
 (peg-parse arith-expr "1+x")
 #f)

(check-equal?
 (peg-parse arith-expr "1*23")
 #f)





(define-peg arith-expr* (seq num op-num*))
(define-peg op-num* (alt (seq op num op-num*)
                         eps))

(check-equal?
 (peg-parse arith-expr* "1")
 "1")

(check-equal?
 (peg-parse arith-expr* "1+23-5")
 "1+23-5")

