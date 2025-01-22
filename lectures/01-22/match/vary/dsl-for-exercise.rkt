#lang racket

(require 2htdp/image 2htdp/universe)

(provide
 (rename-out [make-animation animation])
 in-reverse in-sequence above/anim play-animation
 animation-duration
 vary/fn
 param
 
 (contract-out
  [animation-draw
   #;(-> animation? (-> time/c image?))
   (->i ([a animation?]) [result (a) (-> (time-for-anim/c a) image?)])]
  [multiply-speed
   (-> animation? (and/c rational? (>/c 0)) animation?)]
  [ease/anim
   (-> animation? (-> zero-to-one/c zero-to-one/c) animation?)])

 ease-in-sine
 ease-out-sine
 ease-in-out-sine

 in-sequence*)


;; A Param is a (ZeroToOne -> Rational)

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

(define time/c (and/c rational? (>=/c 0)))
(define (time-for-anim/c a) (and/c rational? (>=/c 0) (</c (animation-duration a))))

(define zero-to-one/c (and/c (>=/c 0) (<=/c 1)))

(define (animation-draw-safe anim)
  (define duration (animation-duration anim))
  (lambda (t)
    (unless (< t duration)
      (error 'animation-draw "out of bounds"))
    ((animation-draw anim) t)))

;; An Animation is a...
;;    (animation (-> Rational Image) Rational)
(struct animation [draw duration]
  #:extra-constructor-name make-animation)
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
    [stop-when (lambda (frame) (>= (frame->t frame) (animation-duration anim)))]))

;; An Easing is a (-> Rational Rational)
;; where both input and output are in [0, 1].

;; Animation, Easing -> Animation
;; Transform the timing of an animation with an easing function.
(define (ease/anim anim easing)
  (animation
   (lambda (t)
     ((animation-draw anim) (easing t)))
   (animation-duration anim)))

(define (ease-in-sine x)
  (- 1 (cos (* x (/ pi 2)))))

(define (ease-out-sine x)
  (sin (* x (/ pi 2))))

(define (ease-in-out-sine x)
  (- 1 (/ (cos (* x pi)) 2)))

;; Animation ... -> Animation
(define (in-sequence* . anims)
  (list->animation-sequence anims))

;; (ListOf Animation) -> Animation
;; Requires a list with at least one element.
(define (list->animation-sequence anims)
  (if (= (length anims) 1)
      (first anims)
      (in-sequence (first anims) (list->animation-sequence (rest anims)))))

