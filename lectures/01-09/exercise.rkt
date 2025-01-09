#lang racket

(require 2htdp/image
         threading
         (rename-in "core-opaque.rkt" [in-sequence in-sequence/2]))

;; Animation ... -> Animation
(define (in-sequence . anims)
  (list->animation-sequence anims))

;; (ListOf Animation) -> Animation
;; Requires a list with at least one element.
(define (list->animation-sequence anims)
  (if (= (length anims) 1)
      (first anims)
      (in-sequence/2 (first anims) (list->animation-sequence (rest anims)))))

;; (Integer -> Animation), Integer -> Animation
(define (repeat/i make-anim n)
  (list->animation-sequence
   (for/list ([i (range n)])
     (make-anim i))))

(define square-fill-animation
  (animation (lambda (t) (overlay (scale (add1 (* t 20))
                                         (square 1 "solid" "blue"))
                                  (empty-scene 100 100 "transparent")))
             5))

(play-animation square-fill-animation)



(define (vary make-anim #:from from #:to to #:duration duration)
  (error 'vary "Not implemented yet"))

(define square-fill-animation/2
  (vary 
   (lambda (scale) (overlay (scale scale
                                   (square 1 "solid" "blue"))
                            (empty-scene 100 100 "transparent")))
   #:from 1
   #:to 100
   #:duration 5))

;; How can we make a form like vary that works for any number of parameters?
;; Invent an embedding.

