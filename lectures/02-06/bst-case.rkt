#lang racket

(require (for-syntax syntax/parse racket/syntax))

(struct bst [])
(struct empty-bst bst [])
(struct bst-node bst [val left right])

;; (bst-case <expr>
;;   [(empty-bst) <expr>]
;;   [(bst-node <pat> <pat> <pat>) <expr>]
;; where <pat>s are arbitrary `match` patterns.
;;
;; A pattern matching form specialized to BSTs that
;; requires that all variants be matched.
;;
;; Example:
#;(bst-case x
            [(empty-bst) 'empty]
            [(bst-node v l r) v])
;; Expands to:
#;(match x
    [(empty-bst) 'empty]
    [(bst-node v l r) v])
;;
;; Example:
#;(bst-case x
            [(bst-node v l r) v])
;; Raises a compile-time error.

(begin-for-syntax
  (define (variant-case-transformer variant-names)
    (lambda (stx)
      (syntax-parse stx
        [(_ target:expr
            [(variant:id pat ...) rhs]
            ...)
         (check-matched-variants!
          (attribute variant)
          variant-names
          stx)
         #'(match target
             [(variant pat ...) rhs]
             ...)]))))

(define-syntax bst-case
  (lambda (stx)
    (variant-case-transformer (list #'empty-bst #'bst-node))))


(begin-for-syntax
  ;; (ListOf Identifier), (ListOf Identifier), Syntax -> Void
  (define (check-matched-variants! matched-variants declared-variants ctx)
    (when (not (and (= (length matched-variants) (length declared-variants))
                    (andmap free-identifier=? matched-variants declared-variants)))
      (raise-syntax-error #f "variants incorrectly matched" ctx))))


