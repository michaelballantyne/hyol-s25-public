#lang racket

(provide (for-space peg (all-defined-out))
         (all-from-out "spec.rkt"))

(require syntax-spec-v3 "spec.rkt" (for-syntax syntax/parse))

(define-dsl-syntax ? peg-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ p)
       #'(alt2 p eps)])))

(define-dsl-syntax seq peg-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ p) #'p]
      [(_ p p* ...)
       #'(seq2 p (seq p* ...))])))

(define-dsl-syntax alt peg-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ p) #'p]
      [(_ p p* ...)
       #'(alt2 p (alt p* ...))])))
