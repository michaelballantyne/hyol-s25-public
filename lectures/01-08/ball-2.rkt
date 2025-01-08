#lang racket

(require 2htdp/universe 2htdp/image)

(define BALL-RADIUS 20)
(define SCENE-WIDTH 200)

(define (ball-in-scene left-to-right-0-to-1)
  (define ball (circle BALL-RADIUS "solid" "red"))
  (define scene (empty-scene SCENE-WIDTH (* 2 BALL-RADIUS) "transparent"))

  (define min-x BALL-RADIUS)
  (define max-x (- SCENE-WIDTH BALL-RADIUS))
  (define x (+ min-x (* (- max-x min-x) left-to-right-0-to-1)))
  
  (place-image ball x BALL-RADIUS scene))

(define FPS 28)
(define DURATION 1)

(define (draw-frame frame)
  (ball-in-scene (/ frame FPS)))

(define (should-stop? frame)
  (> (/ frame FPS) DURATION))

(big-bang 0
  [on-tick add1]
  [to-draw draw-frame]
  [stop-when should-stop?])