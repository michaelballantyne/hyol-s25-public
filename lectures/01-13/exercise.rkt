#lang racket

(require 2htdp/image
         threading
         "dsl-for-exercise.rkt")

(define SCENE-WIDTH  200)
(define SCENE-HEIGHT 300)
(define GROUND-Y 250)

(define HIGHEST-BOUNCE 200)
(define HIGHEST-BOUNCE-DURATION 1)

(define X-CENTER (/ SCENE-WIDTH 2))
(define BALL-RADIUS 15)

(define SHADOW-X-OFFSET 5)
(define SHADOW-Y-OFFSET 10)

(define ball (circle BALL-RADIUS "solid" "blue"))
(define shadow (ellipse BALL-RADIUS (/ BALL-RADIUS 2) "solid" "lightgray"))


(define (up max-height)
  ;; It would be nice to specify this animation as something like...
  ;; 
  ;;    An animation of duration HIGHEST-BOUNCE-DURATION where
  ;;      ball-ht varies from 0 to max-height
  ;;      shadow-scale varies from 2 to 3
  ;;    in
  ;;      (~>> (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "white")
  ;;           (place-image (scale shadow-scale shadow)
  ;;                        (+ X-CENTER SHADOW-X-OFFSET)
  ;;                        (+ GROUND-Y  SHADOW-Y-OFFSET))
  ;;           (place-image ball
  ;;                        X-CENTER
  ;;                        (- GROUND-Y ball-ht)))
  ;;
  ;; Invent and implement an addition to the animation DSL to express this idea.
  ;; Consider generalizations.
  (animation
   (lambda (t)
     (define ball-ht (* max-height (/ t HIGHEST-BOUNCE-DURATION)))
     (define shadow-scale (+ 2 (/ t HIGHEST-BOUNCE-DURATION)))
     (~>> (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "white")
          (place-image (scale shadow-scale shadow)
                       (+ X-CENTER SHADOW-X-OFFSET)
                       (+ GROUND-Y  SHADOW-Y-OFFSET))
          (place-image ball
                       X-CENTER
                       (- GROUND-Y ball-ht))))
   HIGHEST-BOUNCE-DURATION))

(define (bounce height)
  (in-sequence
   (ease/anim (up height) ease-out-sine)
   (in-reverse (ease/anim (up height) ease-out-sine))))

(define (scaled-bounce i)
  (multiply-speed (bounce (* HIGHEST-BOUNCE (expt 0.7 i)))
                  (+ 1 (* 0.6 i))))


(define (6-bounces height)
  (apply in-sequence*
    (for/list ([i (in-range 6)])
      (scaled-bounce i))))

(play-animation
 (6-bounces HIGHEST-BOUNCE))


