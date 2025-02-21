#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse))

(syntax-spec
  (binding-class peg-nt)
  (extension-class peg-macro)
  
  (nonterminal peg
    #:description "PEG expression"
    #:binding-space peg
    #:allow-extension peg-macro
    
    eps
    nt:peg-nt
    c:char
    (char-pred e:racket-expr)
    (alt2 p1:peg p2:peg)
    (seq2 p1:peg p2:peg)
    (* p:peg)
    )

  (host-interface/expression
    (peg-parse p:peg e:racket-expr)
    
    (compile-peg #'p)

    #''(peg-parse p e)))


#;(peg-parse (seq2 #\a (seq2 (alt2 #\b eps) #\c))
           "abc")

(peg-parse (seq #\a (? #\b) #\c)
           "abc")


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








#;(binop - (binop - (binop + 1 2) 3) 5)
#;(define-peg arith-expr
  (=> (seq (: n1 num) (* (seq (: op* op) (: n* num))))
      (left-associate-binops n1 op* n*)))





