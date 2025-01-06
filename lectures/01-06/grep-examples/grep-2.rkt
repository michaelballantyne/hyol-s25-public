#lang racket

#|

grep [-rc] [--include pattern] [--exclude pattern] pattern path

Searches files in the given path, looking for text that matches the pattern.

Supports the following options:

-r
     Recursively search subdirectories listed.

-c
     Only a count of selected lines is written to standard output.

--include pat
     If specified, only files matching the given filename pattern are
     searched.

--exclude pat
     If specified, it excludes files matching the given filename
     pattern from the search.

Note that --include and --exclude patterns are processed in the order given.
If a name matches multiple patterns, the latest matching rule wins.
|#

;; A Filter is one of:
;;   (include String)
;;   (exclude String)

(struct include (pat))
(struct exclude (pat))

;; Parse the documented arguments for grep, returning values for the 
;; positional pattern and path arguments, the recursive and count options,
;; and the filters.
;;
;; (List String) -> (values String String Boolean Boolean (List Filter))
(define (parse-arguments
         args
         [recursive? #f] [count? #f] [filters '()])
  (cond
    [(empty? args)
     (error 'parse-arguments "missing positional arguments")]
    [(equal? "-r" (first args))
     (parse-arguments (rest args) #t count? filters)]
    [(equal? "-c" (first args))
     (parse-arguments (rest args) recursive? #t filters)]
    [(equal? "--include" (first args))
     (define pat (second args))
     (define remaining-args (rest (rest args)))
     (define new-filters (append filters (list (include pat))))
     (parse-arguments remaining-args recursive? count? new-filters)]
    [(equal? "--exclude" (first args))
     (define pat (second args))
     (define remaining-args (rest (rest args)))
     (define new-filters (append filters (list (exclude pat))))
     (parse-arguments remaining-args recursive? count? new-filters)]
    [else (values (first args) (second args) recursive? count? filters)]))

(define-values (pattern path recursive? count? filters)
  (parse-arguments
   (vector->list (current-command-line-arguments))))

pattern
path
recursive?
count?
filters