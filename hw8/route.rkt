#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse racket))

;; (router <expr> <expr> <route> ...)
;; route            := (<method> <path> <expr>)
;; path-pat         := [<path-segment-pat> ...]
;; path-segment-pat := <id>
;;                  | (number-parameter <id>)
;;                  | (string-parameter <id>)
;;
;; The pattern variable names provided as part of string and number
;; parameters of a route should be bound within the Racket expression
;; that is the action of the route.
;;
;; A given pattern variable name must only occur once in a path pattern.
;;
;; A use with a given method and path dispatches to a route when the
;; method of the route matches and the path matches the path pattern.
;; A path pattern matches when each segement of the path matches each
;; segment of the path pattern. Path segment patterns match as follows:
;;  - A literal identifier matches a path segment that is a string with the same
;;    contents as the symbol's string
;;  - A string parameter matches any path segment. The pattern variable is bound
;;    to the string containing that path segment.
;;  - A number parameter matches a path segment that can be parsed to a number
;;    via number->string. The pattern variable is bound to the number.
;;
;; It is an error to specify an ordering of routes such that one of
;; the routes can never be dispatched to because a previous route
;; handles all the same paths.
;;
;; Example:
#;(define (route1 method path)
    (router
     method path
     (get [blog]
          'list-articles)
     (get [blog (number-parameter foobar)]
          (list 'numbered article-no))
     (get [blog latest]
          'latest)
     (get [blog (string-parameter article-name)]
          (list 'named article-name))))

(syntax-spec
  (host-interface/expression
    (router method:racket-expr path:racket-expr r:route ...)
    (check-unreachable-routes! (attribute r))
    #'(compile-routes method path r ...))

  (nonterminal route
    (m:method [pf:path-fragment ...]
              body:racket-expr)
    #:binding (scope (import pf) ... body))
  
  (nonterminal method
    get
    post
    put
    delete)  

  (nonterminal/exporting path-fragment
    #:description "path fragment"
    
    (number-parameter path-var:racket-var)
    #:binding (export path-var)

    (string-parameter path-var:racket-var)
    #:binding (export path-var)
    
    lit:id)
  )


(define-syntax compile-routes
  (syntax-parser
    [(_ method path-segments 
        (route-method [route-path ...] handler)
        ...)

     (define/syntax-parse
       (match-pat-for-route ...)
       (for/list ([path (attribute route-path)])
         (compile-path-to-pattern path)))
       
     #'(match (list method path-segments)
         [(list 'route-method match-pat-for-route)
          handler]
         ...
         [_ (error 'router "no matching route")])]))


;; Compile-time Racket code used to compile patterns and check for overlapping

(begin-for-syntax
  ;; (ListOf PathElementSyntax) -> PatternSyntax
  ;; Example:
  #;#'[blog (number-parameter article-no)]
  ;; ->
  #;#'(list "blog" (app string->number (? number? article-no)))
  (define (compile-path-to-pattern path-elements)
    (define/syntax-parse (pattern-for-path-element ...)
      (for/list ([path-element path-elements])
        (compile-path-element-to-pattern path-element)))
    #'(list pattern-for-path-element ...))

  ;; PathElementSyntax -> PatternSyntax

  ;; Example:
  #;blog
  ;; ->
  #;"blog"
  
  ;; Example:
  #;(number-parameter article-no)
  ;; ->
  #;(app string->number (? number? article-no))
  (define (compile-path-element-to-pattern path-element)
    (syntax-parse path-element
      #:datum-literals (number-parameter string-parameter)
      [(number-parameter x)
       #'(app string->number (? number? x))]
      [(string-parameter x)
       #'x]
      [literal:id
       (symbol->string (syntax->datum #'literal))]))

  ;; (ListOf RouteSyntax) -> Void or error
  (define (check-unreachable-routes! routes)
    (define unreachable-route (first-unreachable-route routes))
    (when unreachable-route 
      (raise-syntax-error 'router "unreachable route" unreachable-route)))

  ;; (ListOf RouteSyntax) -> (or RouteSyntax #f)
  (define (first-unreachable-route routes)
    (match routes
      ['() #f]
      [(cons a d)
       (or
        (for/or ([later-route d])
          (and (route-subsumes a later-route)
               later-route))
        (first-unreachable-route d))]))

  ;; RouteSyntax, RouteSyntax -> Boolean
  (define (route-subsumes r1 r2)
    (define/syntax-parse (method1 (path-pat1 ...) _) r1)
    (define/syntax-parse (method2 (path-pat2 ...) _) r2)
    (and
     (equal? (syntax->datum #'method1)
             (syntax->datum #'method2))
     (path-subsumes (attribute path-pat1)
                    (attribute path-pat2))))

  ;; (ListOf PathSegmentSyntax), (ListOf PathSegmentSyntax) -> Boolean
  (define (path-subsumes path-segments1 path-segments2)
    (and (= (length path-segments1)
            (length path-segments2))
         (for/and ([segment1 path-segments1]
                   [segment2 path-segments2])
           (path-segment-subsumes segment1 segment2))))

  ;; PathSegmentSyntax, PathSegmentSyntax -> Boolean
  (define (path-segment-subsumes segment1 segment2)
    (syntax-parse (list segment1 segment2)
      [(lit1:id lit2:id)
       (equal? (syntax->datum #'lit1)
               (syntax->datum #'lit2))]
      [(((~datum string-parameter) _) _)
       #t]
      [(((~datum number-parameter) _) ((~datum number-parameter) _))
       #t]
      [_ #f])))


(module+ test
  (require rackunit syntax/macro-testing)
  
  (define (enumerate-articles)               '(TODO enumerate-articles))
  (define (show-numbered-article article-no) `(TODO (show-numbered-article ,article-no)))
  (define (show-named-article article-name)  `(TODO (show-named-article ,article-name)))
  (define (latest-article)                   '(TODO latest-article))

  (define (route1 method path)
    (router
     method path
     (get [blog]
          (enumerate-articles))
     (get [blog (number-parameter article-no)]
          (show-numbered-article article-no))
     (get [blog latest]
          (latest-article))
     (get [blog (string-parameter article-name)]
          (show-named-article article-name))))

  (check-equal?
   (route1 'get (list "blog"))
   '(TODO enumerate-articles))

  (check-equal?
   (route1 'get (list "blog" "5"))
   '(TODO (show-numbered-article 5)))

  (check-equal?
   (route1 'get (list "blog" "foo"))
   '(TODO (show-named-article "foo")))

  (check-equal?
   (route1 'get (list "blog" "latest"))
   '(TODO latest-article))

  (check-equal? (phase1-eval
                 (first-unreachable-route
                  (list #'(get [blog latest]
                               (latest-article))
                        #'(get [blog (string-parameter article-name)]
                               (show-named-article article-name)))))
                #f)

  (check-equal? (phase1-eval
                 (first-unreachable-route
                  (list #'(get [blog (string-parameter article-name)]
                               (show-named-article article-name))
                       
                        #'(get [blog latest]
                               (latest-article)))))
                '(get [blog latest]
                              (latest-article)))
                
  
  (check-equal?
   (phase1-eval (path-subsumes (list #'blog #'(number-parameter article-no))
                               (list #'blog #'(string-parameter article-name))))
   #f)
  
  (check-equal?
   (phase1-eval (path-subsumes (list #'blog #'(string-parameter article-name))
                               (list #'blog #'(number-parameter article-no))))
   #t)
  
  (check-exn
   #rx"unreachable route"
   (lambda ()
     (convert-compile-time-error
      (lambda (method path)
        (router
         method path
         (get [blog]
              (enumerate-articles))
         (get [blog latest]
              (latest-article))
         (get [blog (string-parameter article-name)]
              (show-named-article article-name))
         (get [blog (number-parameter article-no)]
              (show-numbered-article article-no))
         )))))
  
  (check-exn
   #rx"unreachable route"
   (lambda ()
     (convert-compile-time-error
      (lambda (method path)
        (router
         method path
         (get [blog]
              
              (enumerate-articles))
         (get [blog (number-parameter article-no)]
              (show-numbered-article article-no))
         (get [blog (string-parameter article-name)]
              (show-named-article article-name))
         (get [blog latest]
              (latest-article))))))))

