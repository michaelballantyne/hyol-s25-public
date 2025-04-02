#lang racket

(provide compile-query)

(require "ast.rkt" (prefix-in rt: "core-streams.rkt"))

;; QueryAST -> Syntax
;;
;; Compile from a QueryAST to a syntax object containing Racket code
;; that uses the functions from the shallow embedding to compute the
;; query. Evaluating the Racket code will produce a table representing
;; the query's result.
(define (compile-query q)
  (match q
    [(ast:query from-clause clauses)
     #`(rt:query/rows
        #,(compile-from-clause from-clause)
        #,@(map compile-clause clauses))]))

;; FromClauseAST -> Syntax
(define (compile-from-clause fc)
  (match fc
    [(ast:from table-val field-names)
     #`(rt:from '#,table-val '#,field-names)]))

;; ClauseAST -> Syntax
(define (compile-clause c)
  (match c
    [(ast:join table-val field-names col1 col2)
     #`(rt:join '#,table-val '#,field-names '#,col1 '#,col2)]
    [(ast:where cols condition)
     #`(rt:where '#,cols '#,condition)]
    [(ast:select cols)
     #`(rt:select #,@(map (lambda (c) #`'#,c) cols))]
    [(ast:limit n)
     #`(rt:limit #,n)]))
