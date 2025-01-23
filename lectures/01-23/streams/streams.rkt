#lang racket/base

(require rackunit)

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

;; Natural -> (StreamOf Natural)
;; Stream of every other Natural starting from `n`
(define (every-other n)
  (stream-pair n (lambda () (every-other (+ n 2)))))

(stream-first (stream-rest (every-other 0)))

;; (stream-cons e1 e2)
;; ---->
;; (stream-pair e1 (lambda () e2))

(define (every-other n)
  (stream-cons n (every-other (+ n 2))))


