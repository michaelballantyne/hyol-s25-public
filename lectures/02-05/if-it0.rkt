#lang racket

(define fiona (hash 'name "Fiona Brown" 'articles 3))

;; You implemented this...

(and/as (hash-ref fiona 'articles #f) #:as count
        (number? count)
        (= count 3))















;; What if we wanted this?

(and/it (hash-ref fiona 'articles #f)
        (and (number? it)
             (= it 3)))










;; (if/it <expr> <expr> <expr>)
;; Like `if`, but the value of the condition is bound
;; in the consequent as `it`.

(if/it (hash-ref fiona 'articles #f)
       it
       0)




