#lang racket

(require rackunit (for-syntax syntax/parse))

(define-syntax or*
  (lambda (stx)
    (syntax-parse stx
      [(or e1 e2)
       #'(let ([tmp e1])
           (if tmp
               tmp
               e2))])))

(check-equal?
 (let ([tmp 5])
   (or* #f
       tmp))
 5)