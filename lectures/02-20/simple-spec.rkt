#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse))

(syntax-spec

  ;; <peg> := eps
  ;;        | <char>
  ;;        | (char-pred <expr>)
  ;;        | (alt2 <peg> <peg>)
  ;;        | (seq2 <peg> <peg>)
  ;;        | (* <peg>)
  ;;        | <id>
  
  (nonterminal peg
    #:description "PEG expression"   
    eps
    c:char
    (char-pred e:racket-expr)
    (alt2 p1:peg p2:peg)
    (seq2 p1:peg p2:peg)
    (* p:peg))

  ;; <racket-expr> := ....
  ;;                | (peg-parse <peg> <racket-expr>)
  
  (host-interface/expression
    (peg-parse p:peg e:racket-expr)
    #''(peg-parse p e)))

(peg-parse (seq2 #\a (seq2 (alt2 #\b eps) (* #\c)))
           "abccccccccc")

