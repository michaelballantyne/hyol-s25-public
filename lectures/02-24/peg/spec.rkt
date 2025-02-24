#lang racket

(provide (all-defined-out)
         (for-space peg (all-defined-out))
         (for-syntax (all-defined-out)))

(require syntax-spec-v3
         (for-syntax syntax/parse)
         (prefix-in rt: "runtime.rkt")
         (for-syntax "compile.rkt"))

(syntax-spec
  (binding-class peg-nt)
  (extension-class peg-macro #:binding-space peg)

  (nonterminal peg   
    ps:peg-seq
    #:binding (scope (import ps)))

  (nonterminal/exporting peg-seq
    #:allow-extension peg-macro
    #:binding-space peg
    
    (: v:racket-var p:peg)
    #:binding (export v)
    
    (seq2 ps1:peg-seq ps2:peg-seq)
    #:binding [(re-export ps1) (re-export ps2)]
    
    (* ps:peg-seq)
    #:binding (re-export ps)

    pe:peg-el)
  
  (nonterminal peg-el
    #:allow-extension peg-macro
    #:binding-space peg

    eps
    nt:peg-nt
    c:char
    (char-pred e:racket-expr)
    (alt2 p1:peg p2:peg)
    
    (=> ps:peg-seq a:racket-expr)
    #:binding (scope (import ps) a))

  (host-interface/definition
    (define-peg nt:peg-nt p:peg)
    #:binding (export nt)
    #:lhs [#'nt]
    #:rhs [#`(lambda (in) (#,(compile-peg #'p) in))])

  (host-interface/expression
    (peg-parse p:peg e:racket-expr)
    #`(rt:peg-parse #,(compile-peg #'p) e)))
