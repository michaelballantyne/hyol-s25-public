#lang racket

(require (prefix-in rt: "embedded.rkt")
         (for-syntax syntax/parse))

(define-syntax minimatch
  (lambda (stx)
    (syntax-parse stx
      [(_ v:id [pat rhs] clause ...)
       (define/syntax-parse
         (var ...)
         (pattern-vars #'pat))
       
       #'(let ([res (compile-pattern v pat)])
           (if (not (rt:failure? res))
               (apply (lambda (var ...) rhs) (rt:success-var-vals res))
               (minimatch v clause ...)))]
      [(_ v:id)
       #'(error 'match "no clause matched")]
      [(_ e clause ...)
       #'(let ([v e])
           (my-match v clause ...))])))

(begin-for-syntax
  (define (pattern-vars pat)
    (syntax-parse pat
      [((~datum cons) p1 p2)
       (append (pattern-vars #'p1) (pattern-vars #'p2))]
      [((~datum quote) v)
       '()]
      [v:id
       (list #'v)])))

(define-syntax compile-pattern
  (lambda (stx)
    (syntax-parse stx
      [(_ v ((~datum cons) p1 p2))
       #'(if (pair? v)
               (let ([p1-res (let ([v1 (car v)]) (compile-pattern v1 p1))])
                 (if (rt:success? p1-res)
                     (let ([p2-res (let ([v2 (cdr v)]) (compile-pattern v2 p2))])
                       (if (rt:success? p2-res)
                           (rt:success (append (rt:success-var-vals p1-res)
                                               (rt:success-var-vals p2-res)))
                           (rt:failure)))
                     (rt:failure)))
               (rt:failure))]
      [(_ v ((~datum quote) pv))
       #'(if (equal? v 'pv)
               (rt:success (list))
               (rt:failure))]
      [(_ v x:id)
       #'(rt:success (list v))])))

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
