#lang racket

(require 2htdp/image 2htdp/universe)

;; An Animation is a...
;;    (animation (-> Rational Image) Rational)
(struct animation [draw duration])

;; Animation, Rational -> Animation
(define (multiply-speed anim factor)
  (error 'animation-dsl "not implemented yet"))

;; Animation -> Animation
(define (in-reverse anim)
  (define (draw-reversed t)
    (define draw (animation-draw anim))
    (draw (- (animation-duration anim) t)))
  (animation
   draw-reversed
   (animation-duration anim)))

;;
;; TODO: Implement in-sequence and above/anim
;;

;; Animation, Animation -> Animation
(define (in-sequence anim1 anim2)
  (error 'animation-dsl "not implemented yet"))

;; Animation, Animation -> Animation
(define (above/anim anim1 anim2)
  (error 'animation-dsl "not implemented yet"))


(define FPS 28)

;; Animation -> (void)
(define (play-animation anim)
  (define (frame->t n) (/ n FPS))
  (big-bang 0
    [on-tick add1]
    [to-draw (lambda (frame) ((animation-draw anim) (frame->t frame)))]
    [stop-when (lambda (frame) (> (frame->t frame)
                                  (animation-duration anim)))]))

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

  (play-animation (in-reverse ball-anim))

  ;; Make these work:
  #;(play-animation (in-sequence ball-anim (in-reverse ball-anim)))
  
  #;(play-animation (above/anim
                   (in-sequence (in-reverse ball-anim) ball-anim)
                   (in-sequence ball-anim (in-reverse ball-anim))))
  
  )