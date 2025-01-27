#lang racket

(require (for-syntax syntax/parse))

;; Exercise: implement the `cond*` form, which is similar to Racket's
;; `cond`.

;; The grammar is:
;;
;; (cond* <clause> ... <else-clause>)
;;
;; <clause>      := [<expr> <expr>]
;; <else-clause> := [#:else <expr>]

;; Evaluates the test expression in the left-hand-side of
;; each clause in turn. For the first that returns a value that
;; is not #f, the consequent expression in that clause's right
;; hand side is evaluated to produce the result. If all test
;; expressions return #f, the expression in the else clause is
;; evaluated to produce the result.

;; Example:
#;(define (list-of-numbers? l)
    (cond*
     [(empty? l) #t]
     [(pair? l)
      (and (number? (first l)) (list-of-numbers? (rest l)))]
     [#:else #f]))
;; Should expand to:
#;TODO

;; Transformed grammar for matching:
;; TODO

(define-syntax cond*
  (lambda (stx)
    (syntax-parse stx
      [(_ anything ...)
       (error 'cond* "not yet implemented")])))

