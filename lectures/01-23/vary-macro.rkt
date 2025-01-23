#lang racket

(require 2htdp/image
         "animation-dsl.rkt"
         (for-syntax syntax/parse))


;; Rational, Rational -> Param
(define (param from to)
  (lambda (fraction) (+ from (* fraction (- to from)))))

;; (Rational ... -> Image), Rational, Param ... -> Animation
;; An animation that varies multiple parameters according to the given Param functions,
;; using `make-image` to render at each sampled values.
(define (vary/fn make-image #:duration duration . param-fns)
  (animation
    (lambda (t)
      (define fraction (/ t duration))
      (apply make-image (for/list ([param-fn param-fns]) (param-fn fraction))))
    duration))



;; We wish we could write...

(define square-fill-animation
  (vary ([scale-factor #:from 1 #:to 100]
         [angle        #:from 0 #:to 90])
        #:duration 2
        (overlay (rotate
                  angle
                  (scale scale-factor
                         (square 1 "solid" "blue")))
                 (empty-scene 100 100 "transparent"))))

(define-syntax vary
  (lambda (stx)
    (syntax-parse stx
      [(_ ([var #:from from #:to to] ...+)
          #:duration duration
          body)
       #'(vary/fn
          (lambda (var ...)
            body)
          #:duration duration
          (param from to)
          ...)])))

;; And expand to...

#;(define square-fill-animation
  (vary/fn
   (lambda (scale-factor angle)
     (overlay (rotate
               angle
               (scale scale-factor
                      (square 1 "solid" "blue")))
              (empty-scene 100 100 "transparent")))
   #:duration 2
   (param 1 100)
   (param 0 90)))


(play-animation
 square-fill-animation)