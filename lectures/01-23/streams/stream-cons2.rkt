#lang racket

(require rackunit)

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
  (define e1 (second stx))
  (define e2 (third stx))
  (list 'stream-pair e1 (list 'lambda '() e2)))

(check-equal?
 (stream-cons-transform
  '(stream-cons n (every-other (+ n 2))))
 '(stream-pair n (lambda () (every-other (+ n 2)))))