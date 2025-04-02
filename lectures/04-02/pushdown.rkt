#lang racket

(provide predicate-pushdown)

(require "ast.rkt")

;; QueryAST -> QueryAST
;;
;; Optimize the query with predicate pushdown.
(define (predicate-pushdown q)
  (match q
    [(ast:query from-clause clauses)
     (ast:query from-clause (predicate-pushdown/clauses clauses))]))

;; (ListOf ClauseAST) -> (ListOf ClauseAST)
;;
;; Reorder the clauses such that `where` clauses appear just below
;; the join that introduces the last of the columns they depend on.
(define (predicate-pushdown/clauses cs)
  (reverse (for/fold ([reversed (list)])
                     ([c cs])
             (if (ast:where? c)
                 (push-down c reversed)
                 (cons c reversed)))))

;; WhereClauseAST, (ListOf ClauseAST) -> (ListOf ClauseAST)
;;
;; Push c as far into reversed as it can go,
;; without going past anything that binds a referenced column.
(define (push-down c reversed)
  (match reversed
    [(list) (list c)]
    [(cons c^ reversed^)
     (if (can-push-down? c c^)
         (cons c^ (push-down c reversed^))
         (cons c reversed))]))

;; WhereClauseAST, ClauseAST -> Boolean
;;
;; Return whether it is safe to reorder the where-clause to
;; a position before the other given clause.
(define (can-push-down? where-clause other-clause)
  (define bound-vars (get-clause-bound-vars other-clause))
  (define referenced-vars (ast:where-cols where-clause))
  (set-empty? (set-intersect bound-vars referenced-vars)))


;; ClauseAST -> (ListOf Symbol)
;;
;; Return a list of the columns that are bound by this clause.
(define (get-clause-bound-vars c)
  (match c
    [(? ast:join?)
     (ast:join-cols c)]
    [_ '()]))