#lang racket

(require (for-syntax syntax/parse))

(define-syntax for-set
  (lambda (stx)
    (syntax-parse stx
      [(_ ([el rhs])
          body)
       #'(sequence-fold
          (lambda (v el)
            (set-add v body))
          (set)
          rhs)])))

(let ([set '(1 2 -2 3)])
  (for-set ([v set])
    (abs v))
  ;; expands to
  (sequence-fold_d
   (lambda_d (v_d v_u)
     (set-add_d v_d (abs_u v_u)))
   (set_d)
   set_u)
  )












#|

"Binding as Sets of Scopes"

https://users.cs.utah.edu/plt/scope-sets/

|#


#;(let ([set '(1 2 -2 3)])
  #;(for-set ([v set])
    (abs v))
  ;; expands to
  (sequence-fold
   (lambda (v v)
     (set-add v (abs v)))
   (set)
   set)

  (sequence-fold_d
   (lambda (v_d v_u)
     (set-add_d v_d (abs_d v_u)))
   (set_d)
   set_u)
  )
