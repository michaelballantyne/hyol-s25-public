#lang racket

(require 2htdp/image)

(define BALL-RADIUS 20)
(define SCENE-WIDTH 200)

(define (ball-in-scene left-to-right-0-to-1)
  (define ball (circle BALL-RADIUS "solid" "red"))
  (define scene (empty-scene SCENE-WIDTH (* 2 BALL-RADIUS) "transparent"))

  (define min-x BALL-RADIUS)
  (define max-x (- SCENE-WIDTH BALL-RADIUS))
  (define x (+ min-x (* (- max-x min-x) left-to-right-0-to-1)))
  
  (place-image ball x BALL-RADIUS scene))

(ball-in-scene 0)
(ball-in-scene 0.5)
(ball-in-scene 1)