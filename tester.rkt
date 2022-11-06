#lang racket
(require rackunit)
(require "interp.rkt")

(check-equal? (expand-args '((1 5) 6 7)) '(1 2 3 4 5 6 7))


(for ([file (filter (lambda (s) (string-suffix? s ".rkt"))(map path->string (directory-list "tests")))])
    (define file_name (string-append "tests/" file)) 
    (check-equal? (run-on-file file_name) (string-trim (file->string (string-append file_name ".qasm")))))