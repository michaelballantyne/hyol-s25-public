#lang racket

(provide (all-defined-out))

(define FPS 28)
(define (play-animation draw-frame duration)
  (define (draw-frame frame)
    (ball-in-scene (/ frame FPS)))

  (define (should-stop? frame)
    (> (/ frame FPS) duration))

  (big-bang 0
    [on-tick add1]
    [to-draw draw-frame]
    [stop-when should-stop?]))