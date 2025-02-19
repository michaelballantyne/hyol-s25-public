#lang racket

(require "peg-embedded.rkt")

(define-peg digit (char-pred char-numeric?))

(define-peg num (seq digit (p* digit)))
(peg-parse num "123")


;; An alternative with recursion
(define-peg num2 (alt (seq digit num2)
                      digit))
(peg-parse num2 "123")


;; Yet another way with recursion
(define-peg num3 (alt (seq num3 digit)
                      digit))
#;(peg-parse num3 "123")

;; A recursion is problematic even if there's something before it---
;; if that thing can match the empty string.
(define-peg num4 (alt (seq (alt eps digit) num3)
                      digit))

(peg-parse num4 "123")