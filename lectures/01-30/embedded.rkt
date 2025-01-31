#lang racket

(provide (all-defined-out))

(module+ test
  (require rackunit))

;; Match as an embedded DSL.

#;(match/fn (list 2 1)
     (clause (list/p (==/p 1) var/p)
             (lambda (b)
               (format "matched first with b: ~a" b)))
     (clause (list/p var/p (==/p 1))
             (lambda (a)
               (format "matched second with a: ~a" a)))
     (clause (list/p var/p var/p)
             (lambda (a b)
               (format "matched third with a: ~a, b: ~a"
                       a b))))


;; A MatchResult is a Success or a Failure

;; A Success is a (success (ListOf Any))
(struct success [var-vals] #:transparent)

;; A Failure is a (failure)
(struct failure [])

;;-----------------------------------------------
;; Patterns

;; A Pattern is a (Any -> MatchResult)

;; Pattern
(define var/p
  ;; Any -> (or Success Failure)
  (lambda (v)
    (success (list v))))

;; Any -> Pattern
(define (==/p pv)
  ;; Any -> (or Success Failure)
  (lambda (v)
    (if (equal? v pv)
        (success (list))
        (failure))))

;; Pattern, Pattern -> Pattern
(define (cons/p p1 p2)
  ;; Any -> (or Success Failure)
  (lambda (v)
    (if (pair? v)
        (let ([v1 (car v)] [v2 (cdr v)])
          (let ([v1-match-res (p1 v1)])
            (if (failure? v1-match-res)
                (failure)
                (let ([v2-match-res (p2 v2)])
                  (if (failure? v2-match-res)
                      (failure)
                      (success
                       (append
                        (success-var-vals v1-match-res)
                        (success-var-vals v2-match-res))))))))
        (failure))))


;; Pat ... -> Pat
(define (list/p . ps)
  (if (empty? ps)
      (==/p '())
      (cons/p (first ps)
              (apply list/p (rest ps)))))


;;-----------------------------------------------
;; Clauses

;; A Clause is (clause Pattern (-> (Any ...) Any))
(struct clause [pat body])

;; Any Clause -> (or Failure Any)
(define (try-clause v c)
  (define pat (clause-pat c))
  (define body (clause-body c))
    
  (define res (pat v))
  (if (success? res)
      (apply body (success-var-vals res))
      res))

(module+ test
  (check-equal?
   (try-clause (list 1 2)
               (clause (list/p (==/p 1) var/p)
                       (lambda (b)
                         (format "matched first with b: ~a" b))))
   "matched first with b: 2"))

;;-----------------------------------------------
;; Match

;; Any, Clause ... -> Any
;; Try matching the value against each clause in turn.
;; If no clause matches, raise an error.
(define (match/fn v . clauses)
  (cond
    [(empty? clauses)
     (error 'minimatch "no clause matched")]
    [else
     (define c (first clauses))
     (define res (try-clause v c))
     
     (if (not (failure? res))
         res
         (apply match/fn v (rest clauses)))]))



(module+ test
  (require rackunit)
  (check-equal?
   (match/fn
       (list 2 1)
     (clause (list/p (==/p 1) var/p)
             (lambda (b)
               (format "matched first with b: ~a" b)))
     (clause (list/p var/p (==/p 1))
             (lambda (a)
               (format "matched second with a: ~a" a)))
     (clause (list/p var/p var/p)
             (lambda (a b)
               (format "matched third with a: ~a, b: ~a" a b))))
   "matched second with a: 2"))

