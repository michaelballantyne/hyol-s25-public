#lang racket/base

(require syntax-spec/tests/dsls/cmdline/cmdline
         syntax-spec/tests/dsls/cmdline/sugar)

(struct include [pattern] #:transparent)
(struct exclude [pattern] #:transparent)

(define/command-line-options
  #:program "grep"
  #:options
  [count? (switch/o "-c" "Print a count only")]
  [recursive? (switch/o "-r" "Search recursively")]
  [filters
   (list/o
    ["--include" pattern "pattern to include" (include pattern)]
    ["--exclude" pattern "pattern to exclude" (exclude pattern)])]
  #:arguments pattern path)

pattern
path
recursive?
count?
filters