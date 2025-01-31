#lang racket

(require "macro.rkt")

(define (do-grep search path include)
  (format "searching for ~a in ~a including ~a"
          search path include))

(match (vector->list (current-command-line-arguments))
  [(cons search (cons path '()))
   (do-grep search path "*")]
  [(cons '"--include"
         (cons include
               (cons search
                     (cons path '()))))
   (do-grep search path include)])

