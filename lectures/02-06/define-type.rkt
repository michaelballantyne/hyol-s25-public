#lang racket

(provide define-type)

(require (for-syntax syntax/parse racket/syntax))

;; (define-type <name:id> <variant> ...)
;; <variant> := (<variant-name:id> <id> ...)
;;
;; Defines:
;;   - a `struct` for the type
;;   - `struct`s for each variant that have the first struct as their supertype.
;;   - a pattern matching form `name-case` that is like match except that it
;;     statically requires the top-level patterns of the match to correspond
;;     to the variants of the type.

;; Example:
#;(define-type bst
  (empty-bst)
  (bst-node val left right))
;; Expands to:
#;(begin
  (struct bst [])
  (struct empty-bst bst [])
  (struct bst-node bst [val left right])
  (define-syntax bst-case (variant-case-transformer (list #'empty-bst #'bst-node))))

(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ name:id (variant-name:id field-name:id ...) ...)
     (define/syntax-parse variant-case
       (format-id #'name "~a-case" #'name))
     #'(begin
         (struct name [])
         (struct variant-name name [field-name ...])
         ...
         (define-syntax variant-case
           (variant-case-transformer (list #'variant-name ...))))]))


(begin-for-syntax
  ;; (ListOf Identifier) -> (Syntax -> Syntax)
  (define (variant-case-transformer variants)
    (lambda (stx)
      (syntax-parse stx
        [(_ e
            [(matched-variant-name:id pats ...) rhs]
            ...)
         (check-matched-variants!
          (attribute matched-variant-name)
          variants
          stx)
         #'(match e
             [(matched-variant-name pats ...) rhs]
             ...)])))
  
  ;; (ListOf Identifier) (ListOf Identifier) Syntax -> Void
  (define (check-matched-variants! matched-variants declared-variants ctx)
    (when (not (and (= (length matched-variants) (length declared-variants))
                    (andmap free-identifier=? matched-variants declared-variants)))
      (raise-syntax-error #f "variants incorrectly matched" ctx))))
