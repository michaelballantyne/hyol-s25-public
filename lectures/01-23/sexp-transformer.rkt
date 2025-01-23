#lang racket

(require (for-syntax racket))
(provide (for-syntax sexp-transformer (all-from-out racket)))

(begin-for-syntax
  (define (sexp-transformer f)
    (lambda (stx)
      (datum->syntax stx (f (syntax->datum stx))))))