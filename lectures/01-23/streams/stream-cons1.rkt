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

(define (stream-cons-transformer stx)
  (match stx
    [(list 'stream-cons e1 e2)
     (list 'stream-pair e1 (list 'lambda '() e2))]))

(check-equal?
 (stream-cons-transformer
  '(stream-cons n (every-other (+ n 2))))
 '(stream-pair n (lambda () (every-other (+ n 2)))))
