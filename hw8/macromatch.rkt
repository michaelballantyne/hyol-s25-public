#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse)
         "inlined.rkt")

(syntax-spec
  (extension-class pattern-macro #:binding-space pattern)
  
  (nonterminal/exporting pat
    #:description "match pattern"
    #:allow-extension pattern-macro
    #:binding-space pattern

    x:racket-var
    #:binding (export x)

    (cons p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)]
    
    (== e:racket-expr))

  (host-interface/expression
    (macromatch e:racket-expr [p:pat rhs:racket-expr] ...)
    #:binding [(scope (import p) rhs) ...]

    #'(minimatch e [p rhs] ...)))


(define-dsl-syntax quote pattern-macro
  (syntax-parser
    [(_ lit)
     #'(== 'lit)]))

(define-dsl-syntax list pattern-macro
  (syntax-parser
    [(_) #''()]
    [(_ p0 p ...)
     #'(cons p0 (list p ...))]))

(define-dsl-syntax quasiquote pattern-macro
  (syntax-parser
    [(_ x:id) #''x]
    [(_ ((~datum unquote) p)) #'p]
    [(_ (qp ...)) #'(list (quasiquote qp) ...)]
    [(_ lit) #''lit]))


(module+ test
  (require rackunit)
  
  (define (f x)
    (macromatch x
     [(cons (== (+ 0 1)) (cons b (== '())))
      (format "matched first with b: ~a" b)]
     [(cons a (cons (== 1) (== '())))
      (format "matched second with a: ~a" a)]
     [(cons a (cons b (== '())))
      (format "matched third with a: ~a, b: ~a" a b)]))

  (check-equal?
   (f (list 1 1))
   "matched first with b: 1")
  
  (check-equal?
   (f (list 2 1))
   "matched second with a: 2")

  (check-equal?
   (f (list 2 2))
   "matched third with a: 2, b: 2")


  (define (g x)
    (macromatch
     x
     [(list '1 b)
      (format "matched first with b: ~a" b)]
     [(list a '1)
      (format "matched second with a: ~a" a)]
     [(list a b)
      (format "matched third with a: ~a, b: ~a" a b)]))

  (check-equal?
   (g (list 1 1))
   "matched first with b: 1")
  
  (check-equal?
   (g (list 2 1))
   "matched second with a: 2")

  (check-equal?
   (g (list 2 2))
   "matched third with a: 2, b: 2")

  (define (h x)
    (macromatch
     x
     [`(1 ,b)
      (format "matched first with b: ~a" b)]
     [`(,a 1)
      (format "matched second with a: ~a" a)]
     [`(,a ,b)
      (format "matched third with a: ~a, b: ~a" a b)]))

  (check-equal?
   (g (list 1 1))
   "matched first with b: 1")
  
  (check-equal?
   (g (list 2 1))
   "matched second with a: 2")

  (check-equal?
   (g (list 2 2))
   "matched third with a: 2, b: 2")
  
  )