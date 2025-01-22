#lang racket

(define (enumerate-articles)      '(TODO enumerate-articles))
(define (show-article article-no) `(TODO (show-article ,article-no)))
(define (latest-article)          '(TODO latest-article))
(define (page-not-found)          '(TODO page-not-found))


(define (route path)
  (match (drop (string-split path "/") 3)
    [(list "blog")
     (enumerate-articles)]
    [(list "blog" article-no)
     (show-article article-no)]
    [(list "blog" "latest")
     (latest-article)]))

(route "http://mballantyne.net/blog")
(route "http://mballantyne.net/blog/5")
(route "http://mballantyne.net/blog/latest")


(define (route2 path)
  (router (drop (string-split path "/") 3)
    [("blog")
     (enumerate-articles)]
    [("blog" article-no)
     (show-article article-no)]
    [("blog" "latest")
     (latest-article)]
    #:not-found (page-not-found)))















(require (for-syntax syntax/parse racket))



(define-syntax router
  (syntax-parser
    [(_ path [path-pat handler] ... #:not-found not-found-handler)
     (let ([overlap (overlapping-routes (syntax->list #'(path-pat ...)))])
       (when overlap 
         (raise-syntax-error 'router "overlapping route" (second overlap))))
     #'(match path
         [(list . path-pat)
          handler]
         ...
         [_ not-found-handler])]))


;; Compile-time Racket code used to check for overlapping

(begin-for-syntax
  (define (overlapping-routes routes)
    (for*/or ([r1 routes]
              [r2 (remove r1 routes)])
      (route-matches r1 r2)))
  
  (define (route-matches r1 r2)
    (and
     (= (length (syntax->datum r1))
        (length (syntax->datum r2)))
     (for/and ([elem1 (syntax->datum r1)]
               [elem2 (syntax->datum r1)])
       (match* (elem1 elem2)
        [(str str) #t]
         [((? symbol?) _) #t]))
     (list r1 r2))))



;; Even nicer errors

(begin-for-syntax
  (define-syntax-class path-pattern
    #:description "path pattern"
    (pattern ((~or el:string e:id) ...))))