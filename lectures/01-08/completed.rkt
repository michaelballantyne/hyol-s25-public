#lang racket

(require 2htdp/image 2htdp/universe)

(provide (all-defined-out))

;; An Animation is a...
;;    (animation (-> Rational Image) Rational)
(struct animation [draw duration])
;; Invariant: only call draw with numbers less than duration.

;; Animation, Rational -> Animation
(define (multiply-speed anim factor)
  (animation
   (lambda (t)
     ((animation-draw anim) (* t factor)))
   (/ (animation-duration anim) factor)))

;; Animation -> Animation
(define (in-reverse anim)
  (define (draw-reversed t)
    (define draw (animation-draw anim))
    (draw (- (animation-duration anim) t)))
  (animation
   draw-reversed
   (animation-duration anim)))

;; Animation, Animation -> Animation
(define (in-sequence anim1 anim2)
  (animation
   (lambda (t)
     (if (<= t (animation-duration anim1))
         ((animation-draw anim1) t)
         ((animation-draw anim2) (- t (animation-duration anim1)))))
   (+ (animation-duration anim1) (animation-duration anim2))))

;; Animation, Animation -> Animation
(define (above/anim anim1 anim2)
  (define duration (max (animation-duration anim1) (animation-duration anim2)))
  (define padded1 (pad-to anim1 duration))
  (define padded2 (pad-to anim2 duration))
  (animation
   (lambda (t)
     (above
      ((animation-draw padded1) t)
      ((animation-draw padded2) t)))
   duration))

;; Animation, Rational -> Animation
;; Extend the length of `anim` to `to-duration` (unless it is already longer)
;; by repeating the last frame during the extra time.
(define (pad-to anim to-duration)
  (animation
   (lambda (t)
     ((animation-draw anim) (min t (animation-duration anim))))
   (max (animation-duration anim) to-duration)))

(define FPS 28)

;; Animation -> (void)
(define (play-animation anim)
  (define (frame->t n) (/ n FPS))
  (big-bang 0
    [on-tick add1]
    [to-draw (lambda (frame) ((animation-draw anim) (frame->t frame)))]
    [stop-when (lambda (frame) (> (frame->t frame) (animation-duration anim)))]))

(module+ main
  (define BALL-RADIUS 20)
  (define SCENE-WIDTH 200)
  
  (define (ball-in-scene left-to-right-0-to-1)
    (define ball (circle BALL-RADIUS "solid" "red"))
    (define scene (empty-scene SCENE-WIDTH (* 2 BALL-RADIUS) "transparent"))

    (define min-x BALL-RADIUS)
    (define max-x (- SCENE-WIDTH BALL-RADIUS))
    (define x (+ min-x (* (- max-x min-x) left-to-right-0-to-1)))
  
    (place-image ball x BALL-RADIUS scene))

  (define ball-anim
    (animation ball-in-scene 1))

  (define bounce
    (in-sequence
     (multiply-speed ball-anim 2/5)
     (in-reverse (multiply-speed ball-anim 2/5))))
  
  (play-animation
   (above/anim
    bounce
    (multiply-speed ball-anim 1/5))))
   
  

  