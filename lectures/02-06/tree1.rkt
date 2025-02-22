#lang racket

(require rackunit)

;; A BST is one of:
;;    (empty-bst)
;;    (bst-node Integer BST BST)

(struct bst [])
(struct empty-bst bst [])
(struct bst-node bst [val left right])


;; BST, Number -> Boolean
;; Checks if `x` is in the BST.
(define (bst-member? tree x)
  (match tree
    [(empty-bst) #f]
    [(bst-node v left right)
     (cond
       [(= x v) #t]
       [(< x v) (bst-member? left x)]
       [else    (bst-member? right x)])]))

(define (leaf val)
  (bst-node val (empty-bst) (empty-bst)))


(define tree
  (bst-node 3
            (leaf 1)
            (bst-node 5
                      (leaf 4)
                      (empty-bst))))

(check-equal?
 (bst-member?
  tree
  4)
 #t)

(check-equal?
 (bst-member?
  tree
  2)
 #f)