#lang racket

(require (for-syntax syntax/parse))
(module+ test (require rackunit))

;; (if/it <expr> <expr> <expr>)
;; Like `if`, but the value of the condition is bound
;; in the consequent as `it`.
;;
;; TODO
(define-syntax if/it
  (lambda (stx)
    (syntax-parse stx
      [(if/it c:expr t:expr e:expr)
       (define/syntax-parse it
         (datum->syntax stx 'it))
       #'(let ([tmp c])
           (if tmp
               (let ([it tmp]) t)
               e))])))


;; (and/it <expr> ...+)
(define-syntax and/it
  (lambda (stx)
    (syntax-parse stx
      [(_ e)
       #'e]
      [(_ e e* ...)
       #'(if/it e
                (and/it e* ...)
                #f)])))

(define fiona (hash 'name "Fiona Brown" 'articles 4))

(and/it (hash-ref fiona 'articles #f)
        (and (number? it)
             (= it 3)))
;; expands to...
#;(if/it (hash-ref fiona 'articles #f)
       (and (number? it)
            (= it 3))
       #f)




      
