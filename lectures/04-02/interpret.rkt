#lang racket

(provide interpret-query)

(require "ast.rkt" (prefix-in rt: "core-streams.rkt"))

;; QueryAST -> Table
;;
;; Interpret the given query AST using the functions from the smallow
;; embedding to perform the query and return the resulting table.
(define (interpret-query q)
  (match q
    [(ast:query from-clause clauses)
     (apply rt:query/rows
            (interpret-from-clause from-clause)
            (map interpret-clause clauses))]))

;; FromClauseAST -> QueryResult
(define (interpret-from-clause fc)
  (match fc
    [(ast:from table-val field-names)
     (rt:from table-val field-names)]))

;; ClauseAST -> (-> QueryResult QueryResult)
(define (interpret-clause c)
  (match c
    [(ast:join table-val field-names col1 col2)
     (rt:join table-val field-names col1 col2)]
    [(ast:where cols condition)
     (rt:where cols condition)]
    [(ast:select cols)
     (apply rt:select cols)]
    [(ast:limit n)
     (rt:limit n)]))
