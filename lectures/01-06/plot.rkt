#lang racket

(require plot)

(plot (list (function sin (- pi) pi)
            (function-label sin (* 1/6 pi) "(1/6 π, 1/2)"
                            #:anchor 'right)))