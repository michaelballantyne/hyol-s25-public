#lang racket

(require "peg-embedded.rkt" rackunit)


(define-peg digit (char char-numeric?))

(define-peg num (seq digit (p* digit)))

(define op (alt (char #\+)
                (char #\-)))


(struct binop [op lhs rhs])


;; It would be nice to express semantic actions that can
;; access parts of sequences...

(define-peg arith-expr
  (=> (seq (: n1 num) (: o op) (: n2 num))
      (binop o n1 n2)))






(define-peg arith-expr*
  (=> (seq (: n1 num) (p* (seq (: o* op) (: n* num))))
      (left-associate-binops n1 o* n*)))
















;; It would be nice if parses like this were fast!

(define-peg comp-op
  (alt "==" ">=" "<=" "<" ">" "!=" "in" "not" "is"))