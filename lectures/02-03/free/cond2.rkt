#lang racket

(require rackunit (for-syntax syntax/parse))

;; (cond* <clauses>)
;; <clauses> := [else <expr>]
;;            | [<expr> <expr>] <clauses>

(define-syntax cond*
  (lambda (stx)
    (syntax-parse stx
      [(_)
       #'(void)]
      [(_ [else-kw rhs])
       #:when (and (identifier? #'else-kw) (free-identifier=? #'else-kw
                                                              #'else))
       #'rhs]
      [(_ [test rhs] clauses ...)
       #'(if test
             rhs
             (cond* clauses ...))])))

(check-equal?
 (cond* [#f 5])
 (void))

(check-equal?
 (cond* [else 5])
 5)

(let ([else #f])
  (check-equal?
   (cond* [else 5])
  (void)))