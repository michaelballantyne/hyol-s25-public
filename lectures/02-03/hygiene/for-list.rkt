#lang racket

(require rackunit (for-syntax syntax/parse))

(define-syntax for-list
  (lambda (stx)
    (syntax-parse stx
      [(_ ([v rhs] ...+)
          body)
       #'(map (lambda (v ...) body) rhs ...)])))


(let ([map (hash 'x 1 'y 2)])
  #;(for-list ([var '(x y)])
    (hash-ref map var))
  ;; expands to
  (map (lambda (var)
         (hash-ref map var))
       '(x y)))

















#;(let ([map (hash 'x 1 'y 2)])
  (map (lambda (var)
         (hash-ref map var))
       '(x y)))