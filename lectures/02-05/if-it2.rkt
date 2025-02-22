#lang racket

(require racket/stxparam (for-syntax syntax/parse))
(module+ test (require rackunit))

;; An auxiliary syntax only valid inside if/it.
(define-syntax-parameter it
  (lambda (stx)
    (raise-syntax-error #f "only valid inside if/it" stx)))

#;(lambda () (+ 1 it))

;; (if/it <expr> <expr> <expr>)
;; Like `if`, but the value of the condition is bound
;; in the consequent as `it`.
(define-syntax if/it
  (lambda (stx)
    (syntax-parse stx
      [(if/it c t e)
       #'(let ([tmp c])
           (if tmp
               (syntax-parameterize ([it (make-it-transformer #'tmp)])
                 t)
               e))])))

(begin-for-syntax
  (define (make-it-transformer tmp-ref)
    (lambda (stx)
      (syntax-parse stx
        [_:id tmp-ref]
        [(it-id rest ...)
         #'(#%app it-id rest ...)]))))



(module+ test
  (define (article-count v)
    (if/it (hash-ref v 'articles #f)
          it
          0))
  
  (check-equal?
   (article-count (hash 'name "Fiona Brown" 'articles 3))
   3)

  (check-equal?
   (article-count (hash 'name "Fiona Brown"))
   0))


;; (and/it <clauses>)
;; <clauses> := <expr>
;;            | <expr> <clauses>
;;
;; Like `and`, but in each <expr> after the first, the value
;; of the previous is bound as `it`.
(define-syntax and/it
  (lambda (stx)
    (syntax-parse stx
      [(_ e)
       #'e]
      [(_ e1 e* ...)
       #'(if/it e1
                (and/it e* ...)
                #f)])))

(module+ test
  (define (article-count2 v)
    (and/it (hash-ref v 'articles #f)
            (and (number? it)
                 it)))

  (check-equal?
   (article-count2 (hash 'name "Fiona Brown" 'articles 3))
   3)

  (check-equal?
   (article-count2 (hash 'name "Fiona Brown" 'articles "none"))
   #f))
