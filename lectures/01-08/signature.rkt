#lang racket

;; We wanted to transform animations by:
;;   speeding up or slowing down
;;   reversing
;;   sequencing
;;   composing with image combinators like `above`

;; Animation, Rational -> Animation
(define (multiply-speed anim factor)
  )

;; Animation -> Animation
(define (in-reverse anim)
  )

;; Animation, Animation -> Animation
(define (in-sequence anim1 anim2)
  )

;; Animation, Animation -> Animation
(define (above/anim anim1 anim2)
  )

;; Animation -> (void)
(define (play-animation anim)
  )