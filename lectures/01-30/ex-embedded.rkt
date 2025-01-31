#lang racket

(require "embedded.rkt")

(define (do-grep search path include)
  (format "searching for ~a in ~a including ~a" search path include))

(match/fn (vector->list
           (current-command-line-arguments))
  (clause (list/p var/p var/p)
          (lambda (search path)
            (do-grep search path "*")))
  (clause (list/p (==/p "--include") var/p var/p var/p)
          (lambda (include search path)
            (do-grep search path include))))

