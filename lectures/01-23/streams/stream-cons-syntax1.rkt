#lang racket

(require rackunit syntax/parse)

;; In general:
;;
;; (stream-cons e1 e2)
;; ---->
;; (stream-pair e1 (lambda () e2))

;; Concretely:
#;(stream-cons n (every-other (+ n 2)))
;; ---->
#;(stream-pair n (lambda () (every-other (+ n 2))))

(define (stream-cons-transform stx)
  (syntax-parse stx
    [(_ e1 e2)
     #'(stream-pair e1 (lambda () e2))]))

(stream-cons-transform
 #'(stream-cons n (every-other (+ n 2))))