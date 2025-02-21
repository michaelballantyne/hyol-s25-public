#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse))

(syntax-spec
  (binding-class peg-var)
  
  (nonterminal peg
    #:description "PEG expression"   
    eps
    c:char
    (char-pred e:racket-expr)
    (alt2 p1:peg p2:peg)
    (seq2 p1:peg p2:peg)

    (seq=> ([x:peg-var p:peg] ...) ref:peg-var)
    #:binding (scope (bind x) ... ref)
    
    (* p:peg))
  
  (host-interface/expression
    (peg-parse p:peg e:racket-expr)
    #''(peg-parse p e)))

(peg-parse (seq=> ([a #\a] [maybe-b (alt2 #\b eps)] [c (* #\c)])
                  maybe-b)
           "abccccccccc")

