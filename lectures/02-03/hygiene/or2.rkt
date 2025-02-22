#lang racket

(require "sexp-transformer.rkt" rackunit)

(define-syntax or*
  (sexp-transformer
   (lambda (stx)
     (match stx
       [`(or* ,e1 ,e2)
        (define fresh-name (gensym))
        `(let ([,fresh-name ,e1])
           (if ,fresh-name
               ,fresh-name
               ,e2))]))))

(check-equal?
 (let ([tmp 5])
   (or* #f
       tmp))
 5)
















#;(define-syntax or*
  (sexp-transformer
   (lambda (stx)
     (match stx
       [`(or* ,e1 ,e2)
        (define tmp (gensym))
        `(let ([,tmp ,e1])
           (if ,tmp
               ,tmp
               ,e2))]))))