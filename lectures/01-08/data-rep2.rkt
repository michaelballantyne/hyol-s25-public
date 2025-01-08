#lang racket

;; An Animation is a...
;;    (animation (-> Rational Image) Rational)
(struct animation [draw duration])

;; Animation, Rational -> Animation
(define (multiply-speed anim factor)
  (error 'animation-dsl "not implemented yet"))

;; Animation -> Animation
(define (in-reverse anim)
  ;; Can we implement this now?
  )

;; Animation, Animation -> Animation
(define (in-sequence anim1 anim2)
  (error 'animation-dsl "not implemented yet"))

;; Animation, Animation -> Animation
(define (above/anim anim1 anim2)
  (error 'animation-dsl "not implemented yet"))

;; Animation -> (void)
(define (play-animation anim)
  (error 'animation-dsl "not implemented yet"))