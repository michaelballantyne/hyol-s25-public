#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse))

(syntax-spec
  (binding-class var)
  (binding-class peg-nt)
  (extension-class peg-macro)

  (nonterminal peg   
    ps:peg-seq
    #:binding (scope (import ps)))

  (nonterminal/exporting peg-seq
    #:allow-extension peg-macro
    
    (: v:var p:peg)
    #:binding (export v)
    
    (seq2 ps1:peg-seq ps2:peg-seq)
    #:binding [(re-export ps1) (re-export ps2)]
    
    (* ps:peg-seq)
    #:binding (re-export ps)

    pe:peg-el)
  
  (nonterminal peg-el
    #:allow-extension peg-macro

    eps
    nt:peg-nt
    c:char
    (char-pred e:racket-expr)
    (alt2 p1:peg p2:peg)
    
    (=> ps:peg-seq a:var)
    #:binding (scope (import ps) a))

  (host-interface/definition
    (define-peg nt:peg-nt p:peg)
    #:binding (export nt)
    #:lhs [#'nt]
    #:rhs [(writeln (syntax->datum #'(define-peg nt p)))
           #'(void)])

  (host-interface/expression
    (peg-parse p:peg e:racket-expr)
    #'(void)))




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



(define-peg digit (char-pred char-numeric?))
(define-peg num (seq digit (* digit)))
(define-peg op (alt #\+ #\-))
(define-peg arith-expr
  (=> (seq (: n1 num) (* (seq (: op* op) (: n* num))))
      op*))