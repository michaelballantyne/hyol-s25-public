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

(define (ball-5-sec t)
  (ball-in-scene (/ t 5)))

(define (ball-2.5-sec t)
  (ball-5-sec (* 2 t)))

;; reverse of ball-2.5-sec
(define (ball-2.5-sec-reverse t)
  (ball-2.5-sec (- 2.5 t)))

;; first ball-2.5-sec, then ball-2.5-sec-reverse
(define (bounce t)
  (if (< t 2.5)
      (ball-2.5-sec t)
      (let ([t (- t 2.5)])
        (ball-2.5-sec-reverse t))))

;; Time -> Image
(define (draw-at-time t)
  (above
   (bounce t)
   (ball-5-sec t)))

(define (draw-frame frame)
  (draw-at-time (/ frame FPS)))

(define (should-stop? frame)
  (> (/ frame FPS) 5))

(big-bang 0
  [on-tick add1]
  [to-draw draw-frame]
  [stop-when should-stop?])
