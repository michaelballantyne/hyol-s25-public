#lang racket

(require (for-syntax syntax/parse))

;; Defines a peg nonterminal.
;;
;; (define-peg <id> <peg>)
;; <peg> := eps
;;        | <char>
;;        | (char-pred <expr>)
;;        | (alt2 <peg> <peg>)
;;        | (seq2 <peg> <peg>)
;;        | (* <peg>)
;;        | <id>
;;
;; Example:
#;(define-peg num (alt2 (seq2 digit num)
                        eps))
(define-syntax define-peg
  (lambda (stx)
    (syntax-parse stx
      [(_ name:id peg)
       (define/syntax-parse compiled-peg
         (compile-peg #'peg))
       #'(define name compiled-peg)])))

(begin-for-syntax
  (define (compile-peg stx)
    (check-peg-syntax! stx)
    (check-left-recursion! stx)
    (generate-code stx))

  (define (check-peg-syntax! stx)
    (syntax-parse stx
      #:datum-literals (eps char-pred alt2 seq2 *)
      [eps #t]
      [c:char #t]
      [(char-pred e) #t]
      [(alt2 p1 p2)
       (check-peg-syntax! #'p1)
       (check-peg-syntax! #'p2)]
      [(seq2 p1 p2)
       (check-peg-syntax! #'p1)
       (check-peg-syntax! #'p2)]
      [(* p)
       (check-peg-syntax! #'p)]
      ;; How can we check that this refers to a valid nonterminal, especially across Racket code and modules?
      [nt:id #t]))

  (define (check-left-recursion! parsed)
    'TODO)

  (define (generate-code parsed)
    #''TODO))




(define-peg digit (char-pred char-numeric?))

;; At this point the nothing notices if we get a reference wrong like `dig`, and
;; the IDE doesn't understand anything.
(define-peg num (seq2 dig (* digit)))
(define-peg op (alt2 #\+ #\-))
(define-peg arith-expr (seq2 num (seq2 op num)))


;; What if we wanted syntactic sugar, like `seq` expanding to `seq2`?
(define-peg arith-expr (seq num op num))

;; We can't just define a macro on top---the Racket expander doesn't
;; expand within our `define-peg` syntax. We'd need to write our own
;; macro expander!
(define-syntax seq
  (lambda (stx)
    (syntax-parse stx
      [(_ p) #'p]
      [(_ p p* ...)
       #'(seq2 p (seq p* ...))])))

