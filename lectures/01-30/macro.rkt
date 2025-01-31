#lang racket

(require (for-syntax syntax/parse)
         (prefix-in rt: "embedded.rkt"))

;; (minimatch <expr> [<pat> <expr>] ...)
;; <pat> := (quote <datum>)
;;        | <id>
;;        | (cons <pat> <pat>)
;;
;; A pattern matcher with a subset of Racket's
;; `match` syntax that compiles to the embedded match DSL.
;;
;; Example:
#;(define (f x)
    (minimatch x
     [(cons '1 (cons b '()))
      (format "matched first with b: ~a" b)]
     [(cons a (cons '1 '()))
      (format "matched second with a: ~a" a)]
     [(cons a (cons b '()))
      (format "matched third with a: ~a, b: ~a" a b)]))
;; Expands to:
#;(define (f x)
    (match/fn x
      (clause (cons/p (==/p 1) (cons/p var/p (==/p '())))
              (lambda (b)
                (format "matched first with b: ~a" b)))
      (clause (cons/p var/p (cons/p (==/p 1) (==/p '())))
              (lambda (a)
                (format "matched second with a: ~a" a)))
      (clause (cons/p var/p (cons/p var/p (==/p '())))
              (lambda (a b)
                (format "matched third with a: ~a, b: ~a" a b)))))


;; (compile-pattern <pat>)
;; <pat> := (quote <datum>)
;;        | <id>
;;        | (cons <pat> <pat>)
;;
;; Compile a match pattern to the embedded match DSL
;; pattern constructors.
;;
;; Example:
#;(compile-pattern (cons '1 (cons b '())))
;; Expands to:
#;(rt:cons/p (rt:==/p '1)
             (rt:cons/p rt:var/p
                        (rt:==/p '())))
(define-syntax compile-pattern
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum quote) datum))
       #'(rt:==/p 'datum)]
      [(_ x:id)
       #'rt:var/p]
      [(_ ((~datum cons) p1 p2))
       #'(rt:cons/p
          (compile-pattern p1)
          (compile-pattern p2))])))











;; (compile-clause [<pat> <expr>])
;;
;; Compile a match clause to the embedded match DSL
;; clause constructor.
;;
;; Example:
#;(compile-clause
   [(cons '1 (cons b '()))
    (format "matched first with b: ~a" b)])
#;(rt:clause
   (compile-pattern (cons '1 (cons b '())))
   (lambda (b)
     (format "matched first with b: ~a" b)))
(define-syntax compile-clause
  (lambda (stx)
    (syntax-parse stx
      [(_ [pat rhs])
       (define/syntax-parse
         (var ...)
         (compute-vars #'pat))
       
       #'(rt:clause
          (compile-pattern pat)
          (lambda (var ...)
            rhs))])))

(begin-for-syntax
  ;; Syntax -> (ListOf Identifier)
  (define (compute-vars pat)
    (syntax-parse pat
      [((~datum quote) datum)
       '()]
      [x:id
       (list #'x)]
      [((~datum cons) p1 p2)
       (append (compute-vars #'p1)
               (compute-vars #'p2))])))






;; (minimatch [<pat> <expr>] ...)
;;
;; The top-level match syntax.
;;
;; Example:
#;(minimatch x
    [(cons '1 (cons b '()))
     (format "matched first with b: ~a" b)]
    [(cons a (cons '1 '()))
     (format "matched second with a: ~a" a)]
    [(cons a (cons b '()))
     (format "matched third with a: ~a, b: ~a" a b)])
;; Expands to:
#;(rt:match/fn
   x
   (compile-clause
    [(cons '1 (cons b '()))
     (format "matched first with b: ~a" b)])
   (compile-clause
    [(cons a (cons '1 '()))
     (format "matched second with a: ~a" a)])
   (compile-clause
    [(cons a (cons b '()))
     (format "matched third with a: ~a, b: ~a" a b)]))

(define-syntax minimatch
  (lambda (stx)
    (syntax-parse stx
      [(_ e clause ...)
       #'(rt:match/fn
          e
          (compile-clause clause)
          ...)])))



(module+ test
  (require rackunit)

  (define (f x)
    (minimatch
     x
     [(cons '1 (cons b '()))
      (format "matched first with b: ~a" b)]
     [(cons a (cons '1 '()))
      (format "matched second with a: ~a" a)]
     [(cons a (cons b '()))
      (format "matched third with a: ~a, b: ~a" a b)]))

  (check-equal?
   (f (list 1 1))
   "matched first with b: 1")
  
  (check-equal?
   (f (list 2 1))
   "matched second with a: 2")

  (check-equal?
   (f (list 2 2))
   "matched third with a: 2, b: 2"))

(module+ main
  (define (f x)
    (minimatch
     x
     [(cons '1 (cons b '()))
      1]
     [(cons a (cons '1 '()))
      2]
     [(cons a (cons b '()))
      3]))

  (define res 0)
  
  (time
   (for ([n (in-range 100000)])
     (set! res (+ res (f (list 1 1))))
     (set! res (+ res (f (list 2 1))))
     (set! res (+ res (f (list 2 2)))))))
