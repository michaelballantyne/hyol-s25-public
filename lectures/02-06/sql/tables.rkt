#lang racket

(require csv-reading)

(provide load-table)

;; Path -> Table
(define (load-table path)
  (with-input-from-file path
    (lambda () (csv-list->table (csv->list (current-input-port))))))
 
(define (csv-list->table lst)
  (define column-names (first lst))
  (define row-lists (rest lst))
  (for/list ([row-list row-lists])
    (for/hash ([column-name column-names]
               [data row-list])
      (values (string->symbol column-name) data))))