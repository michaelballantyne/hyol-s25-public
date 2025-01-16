#lang racket

; an XExpr is one of
;   A String
;   An XNode
; Representing an XML tree
; Example:
(define example-xexpr
  '(*TOP* (html (head (title "My Webpage"))
                (body (p "here is a link:"
                         (a (@ [href "https://google.com"]) "click me!"))
                      (p "here is another link:"
                         (a (@ [href "https://google.com"]) "click me too!"))
                      (br)
                      (p "ok that's all")))))

; An XNode is one of
; (list Symbol XExpr ...)
; (list Symbol (list '@ Attribute ...) XExpr ...)
; Representing an XML tree with a tag (not just free text)

; We can access the data from XExprs with the following API:

; XExpr -> (Listof XExpr)
; This can be used as an XSelector
(define (xexpr-children xexpr) )

; XExpr -> (HashOf Symbol (U #t String))
(define (xexpr-attributes xexpr) )

; XExpr -> String
; inner text of the node, does not include tags
(define (xexpr-text xexpr))
