#lang racket

(define (do-grep search path include)
  (format "searching for ~a in ~a including ~a" search path include))

(let ([args (vector->list
          (current-command-line-arguments))])
  (cond
    [(= (length args) 2)
     (let ([search (first args)] [path (second args)])
       (do-grep search path "*"))]
    [(and (= (length args) 4) (equal? (unsafe-first args) "--include"))
     (let ([include (unsafe-second args)] [search (unsafe-third args)] [path (unsafe-fourth args)])
       (do-grep search path include))]
    [else (error 'match "no matching clause")]))
