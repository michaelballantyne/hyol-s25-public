#lang racket

;; An Animation is a...
;;    (-> Rational Image)

;; Animation, Rational -> Animation
(define (multiply-speed anim factor)
  (error 'animation-dsl "not implemented yet"))

;; Animation -> Animation
(define (in-reverse anim)
  ;; Let's implement this!
  (define (draw-reversed t)
    )
  draw-reversed)

;; Animation, Animation -> Animation
(define (in-sequence anim1 anim2)
  (error 'animation-dsl "not implemented yet"))

;; Animation, Animation -> Animation
(define (above/anim anim1 anim2)
  (error 'animation-dsl "not implemented yet"))

;; Animation -> (void)
(define (play-animation anim)
  (error 'animation-dsl "not implemented yet"))