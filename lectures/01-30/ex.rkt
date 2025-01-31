#lang racket

(define (do-grep search path include)
  (format "searching for ~a in ~a including ~a"
          search path include))

(match (vector->list (current-command-line-arguments))
  [(list search path)
   (do-grep search path "*")]
  [(list "--include" include search path)
   (do-grep search path include)])

