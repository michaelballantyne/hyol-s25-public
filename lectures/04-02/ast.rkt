#lang racket

(provide (all-defined-out))

(struct ast:query [from-clause clauses] #:transparent)
(struct ast:from [table-val cols] #:transparent)
(struct ast:join [table-val cols col1 col2] #:transparent)
(struct ast:where [cols predicate] #:transparent)
(struct ast:select [cols] #:transparent)
(struct ast:limit [n] #:transparent)