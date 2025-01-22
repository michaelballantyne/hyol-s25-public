#lang racket

(require rackunit (for-syntax syntax/parse))

;; Let's define our own streams.

;; A stream is one of:
;;   '()
;;   (stream-pair any (-> () stream))

(struct stream-pair [first-val rest-fn])

(define example-stream (stream-pair 1 (lambda () (stream-pair 2 (lambda () '())))))

(define (stream-empty? s) (null? s))

(define (stream-first s) (stream-pair-first-val s))

;; Stream -> Stream
;; Error if s is '()
(define (stream-rest s)
  (define fn (stream-pair-rest-fn s))
  (fn))


;; (stream-cons v e)
;; ---->
;; (stream-pair v (lambda () e))

(define-syntax stream-cons
  (lambda (stx)
    (syntax-parse stx
      [(_ e1 e2)
       #'(stream-pair e1 (lambda () e2))])))

;; So this:
(define ex2-cons
  (stream-cons 1 (stream-cons 2 '())))

;; Is the same as this:
(define ex2-pairs
  (stream-pair
   1
   (lambda () (stream-pair
               2
               (lambda () '())))))


(check-equal?
 (stream-first (stream-rest ex2-pairs))
 2)

(check-equal?
 (stream-first (stream-rest ex2-cons))
 2)