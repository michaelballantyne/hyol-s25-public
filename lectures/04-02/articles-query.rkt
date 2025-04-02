#lang racket

(require "ast.rkt" "interpret.rkt" "pushdown.rkt" "binding-check.rkt" "surface.rkt")

(define articles
  (list (hash 'id 0
              'title "how to climb a rock wall"
              'body "just go up"
              'author-id 0)
        (hash 'id 1
              'title "why I don't like Racket"
              'body "too many parentheses"
              'author-id 0)
        (hash 'id 2
              'title "why I like Racket"
              'body "all you need is parentheses"
              'author-id 1)))
(define users
  (list (hash 'id 0
              'name "haskell-fan")
        (hash 'id 1
              'name "racket-enjoyer")))

(define example-query
  (ast:query
   (ast:from articles '(author-id title))
   (list
    (ast:join users '(id name)
              'author-id 'id)
    (ast:where '(title) (lambda (title) (equal? title "why I don't like Racket")))
    (ast:select '(name)))))


















#;(interpret-query (predicate-pushdown (check-query example-query)))

#;(query/rows
 (from articles '(author-id title))
 (join users '(id name)
       'author-id 'id)
 (where-equal 'title "why I don't like Racket")
 (select 'name))