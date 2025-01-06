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

(vector->list (current-command-line-arguments))
