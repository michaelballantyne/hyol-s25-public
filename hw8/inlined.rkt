#lang racket

(provide minimatch)

(require (for-syntax syntax/parse))

;; <clause> := [<pat> <expr>]
;; <pat> := (quote <datum>)
;;        | <id>
;;        | (cons <pat> <pat>

;; (minimatch <expr> <clause> ...)
(define-syntax minimatch
  (lambda (stx)
    (syntax-parse stx
      [(_ target clause ...)
       #'(let ([v target])
           (minimatch-val v clause ...))])))

;; (minimatch-val <id> <clause> ...)
(define-syntax minimatch-val
  (lambda (stx)
    (syntax-parse stx
      [(_ target:id)
       #'(error 'minimatch "all patterns failed to match")]
      [(_ target:id
          [pat:expr body:expr]
          clause
          ...)
       #'(let ([fail-k (lambda () 
                          (minimatch-val target
                                         clause
                                         ...))])
           (do-match target pat
                     body
                     fail-k))])))

(define-syntax do-match
  (lambda (stx)
    (syntax-parse stx
      [(do-match target:expr pat:expr on-success:expr fail-k:expr)
       (syntax-parse #'pat
         [((~datum cons) car-pat:expr cdr-pat:expr)
          #'(if (cons? target)
                (let ([car-val (car target)] [cdr-val (cdr target)])
                  (do-match car-val car-pat
                            (do-match cdr-val cdr-pat
                                      on-success
                                      fail-k)
                            fail-k))
                (fail-k))]
         [((~literal ==) e)
          #'(if (equal? target e)
                on-success
                (fail-k))]
         [x:id
          #'(let ([x target]) on-success)])])))

(module+ test
  (require rackunit)

  (define (f x)
    (minimatch
     x
     [(cons (== 1) (cons b (== '())))
      (format "matched first with b: ~a" b)]
     [(cons a (cons (== 1) (== '())))
      (format "matched second with a: ~a" a)]
     [(cons a (cons b (== '())))
      (format "matched third with a: ~a, b: ~a" a b)]))

  (check-equal?
   (f (list 1 1))
   "matched first with b: 1")
  
  (check-equal?
   (f (list 2 1))
   "matched second with a: 2")

  (check-equal?
   (f (list 2 2))
   "matched third with a: 2, b: 2"))
  
