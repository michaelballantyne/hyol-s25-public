#lang racket

(require racket/hash)

(provide (all-defined-out))

;; A Table is a (Listof Row)

;; A QueryResult is a
(struct query-result [data])
;; where data is a (StreamOf Row).
 
;; A ColName is a Symbol
;; A Row is a (HashOf ColName Any)

;; A Clause is a (QueryResult -> QueryResult)

;; QueryResult, Clause ... -> Table
(define (query/rows query-result . clauses)
  (define reordered (predicate-pushdown clauses))
  (query-result->table (apply query query-result reordered)))

(define (predicate-pushdown clauses)
  #;(displayln clauses)
  clauses)

;; QueryResult, Clause ... -> QueryResult
;; Compose a query that transforms the initial query-result (usually produced by `from`),
;; applying the transformation defined by each clause in the order they are provided.
(define (query query-result . clauses)
  (for/fold ([qr query-result])
            ([clause clauses])
    (clause qr)))    

;; Table, (ListOf ColName) -> QueryResult
;; Create a QueryResult from a table of data.
(define (from table cols)
  ((apply select cols) (query-result table)))


;; (ListOf ColName), (Row -> Boolean) -> Clause
;; Filter, keeping only rows that match the predicate.
(define (where cols keep?)
  (lambda (qr)
    (query-result
     (for/stream ([row (query-result-data qr)]
                  #:when (let ([col-vals (for/list ([col cols])
                                           (hash-ref row col))])
                           (apply keep? col-vals)))
       row))))

;; ColName ... -> Clause
;; Keep only the columns specified.
(define (select . col-names)
  (lambda (qr)
    (query-result
     (for/stream ([row (query-result-data qr)])
       (for/hash ([col col-names])
         (values col (hash-ref row col)))))))

;; (Hash ColName ColName) -> Clause
(define (select-and-rename renamings)
  (lambda (qr)
    (query-result
     (for/stream ([row (query-result-data qr)])
       (for/hash ([(old-name new-name) (in-hash renamings)])
         (values new-name (hash-ref row old-name)))))))

;; QueryResult, (Row -> Boolean) -> Clause
;; An inner join of the running query-result with the argument
;; query-result.
(define (join table2 cols col1 col2)
  (lambda (qr1)
    (query-result
     (for*/stream ([row1 (query-result-data qr1)]
                   [row2 (query-result-data (from table2 cols))]
                   #:when (equal? (hash-ref row1 col1) (hash-ref row2 col2)))
       (hash-union row1 row2)))))

;; QueryResult, Integer -> Clause
;; Retain only the first `n` rows of the query result.
(define (limit n)
  (lambda (qr)
    (query-result
     (stream-take (query-result-data qr) n))))

;; ColName (Row -> Any) -> Clause
;; Add to each row a new column called `name` that is computed by applying the procedure to the row.
(define (derived name proc)
  (lambda (qr)
    (query-result
     (for/stream ([row (query-result-data qr)])
       (hash-set row name (proc row))))))

;; QueryResult -> Table
;; Produce a table from the rows of a query result.
(define (query-result->table query-result)
  (stream->list (query-result-data query-result)))



;; Design task part?

(define (list->stream lst)
  (for/stream ([x lst]) x))

;; ColName, (Any, Any -> Boolean) -> Clause
(define (order-by col-name less-than?)
  (lambda (qr)
    (query-result
     (list->stream
      (sort (stream->list (query-result-data qr))
            (lambda (row1 row2)
              (less-than? (hash-ref row1 col-name)
                          (hash-ref row2 col-name))))))))

;; ColName -> Clause
(define (distinct)
  (lambda (qr)
    (query-result
     (stream-remove-duplicates (query-result-data qr) (set)))))

;; Stream, SetEqual -> Stream
(define (stream-remove-duplicates s seen)
  (if (stream-empty? s)
      empty-stream
      (let ([el (stream-first s)])
        (if (set-member? seen el)
            (stream-remove-duplicates (stream-rest s) seen)
            (stream-cons el
                         (stream-remove-duplicates (stream-rest s)
                                                   (set-add seen el)))))))

;; An Aggregator is a (Any ... -> Any)

;; ColName, #:using Aggregator, #:by ColName -> Clause
;;  Groups rows by `by-col-name` and applies the aggregator to the values of `col-name` in each group.
;;  The result only has the col-name and by-col-name columns.
;;
;; Example:
;; (query
;;   (from (list (hash 'author "Alice" 'article-title "A" 'date "2019-01-01")
;;               (hash 'author "Alice" 'article-title "B" 'date "2019-01-02")
;;               (hash 'author "Bob" 'article-title "C" 'date "2019-01-03"))
;;   (aggregate 'article-title #:using count #:by 'author))
;; =>
;; (list (hash 'author "Alice" 'article-title 2)
;;       (hash 'author "Bob" 'article-title 1))
(define (aggregate col-name #:using aggregator #:by by-col-name)
  (lambda (qr)
    (query-result
     (let ()
       (define groups (group-by (lambda (row) (hash-ref row by-col-name)) (stream->list (query-result-data qr))))
       (for/stream ([group groups])
         (define by-col-val (hash-ref (first group) by-col-name))
         (define aggregate-col-vals (map (lambda (row) (hash-ref row col-name)) group))
         (hash by-col-name by-col-val
               col-name (apply aggregator aggregate-col-vals)))))))

;; Define aggregators for count, sum, maximum, and minimum.
 
(define (count . l)
  (length l))

(module+ test
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

  (query/rows
   (from articles '(author-id title))
   (join users '(id name)
         'author-id 'id)
   (where '(title) (lambda (title) (equal? title "why I like Racket")))
   (select 'title 'name)
   (limit 1)))
