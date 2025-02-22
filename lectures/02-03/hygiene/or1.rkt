#lang racket

(require "sexp-transformer.rkt")

(define-syntax or*
  (sexp-transformer
   (lambda (stx)
     (match stx
       [`(or* ,e1 ,e2)
        `(let ([tmp ,e1])
           (if tmp
               tmp
               ,e2))]))))

(let ([tmp 5])
  (or* (read)
       tmp)
  ;; expands to:
  (let ([tmp (read)])
    (if tmp
        tmp
        tmp))
  
  )


















#;(let ([tmp 5])
  (or* (read)
      tmp))

#;(let ([tmp (read)])
  (if tmp
      tmp
      tmp))