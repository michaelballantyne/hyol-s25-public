#lang racket

(provide check-query)

(require "ast.rkt")

;; QueryAST -> QueryAST or error
;;
;; Raise an error when part of a query refers to a column binding,
;; but the column will not in fact be part of the row at that part
;; of the query at runtime.
(define (check-query q)
  (match q
    [(ast:query from-clause clauses)
     (for/fold ([still-bound-ids (ast:from-cols from-clause)])
               ([c clauses])
       (check-clause c still-bound-ids))
     q]))

;; ClauseAST, (ListOf Symbol) -> (ListOf Symbol) or error
;;
;; Check that the clause only refers to the `still-bound-cols` that were bound
;; in earlier clauses, and return an updated set representing the columns that should
;; remain accessible in subsequent clauses.
(define (check-clause c still-bound-cols)
  (match c
    [(ast:select cols)
     (check-refs cols still-bound-cols)
     cols]
    [(ast:where cols body)
     (check-refs cols still-bound-cols)
     still-bound-cols]
    [(ast:join table-val cols col1 col2)
     (check-ref col1 still-bound-cols)
     (check-ref col2 cols)
     (set-union still-bound-cols cols)]
    [(ast:limit n)
     still-bound-cols]))
   
(define (check-refs col-refs still-bound-ids)
  (for ([col col-refs])
    (check-ref col still-bound-ids)))

;; Symbol, (ListOf Symbol) -> Void or error
;; Raise an error if the identifier is not in the set.
(define (check-ref col still-bound-cols)
  (unless (set-member? still-bound-cols col)
    (raise-syntax-error #f "column not available here" col)))
