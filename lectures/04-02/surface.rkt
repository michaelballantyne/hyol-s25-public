#lang racket

(provide (all-defined-out))

(require "ast.rkt" "interpret.rkt" "pushdown.rkt" "binding-check.rkt")

(define (query/rows f . cs)
  (define q
    (ast:query f cs))

  (interpret-query (predicate-pushdown (check-query q))))

(define (from tbl cols)
  (ast:from tbl cols))

(define (where cols f)
  (ast:where cols f))

(define (where-equal col v)
  (where (list col) (lambda (col-v) (equal? col-v v))))

(define (select . cols)
  (ast:select cols))

(define (join tbl cols col1 col2)
  (ast:join tbl cols col1 col2))

(define (limit n)
  (ast:limit n))