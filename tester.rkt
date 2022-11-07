#lang racket
(require rackunit)
(require "interp.rkt")

(check-equal? (generate-args 2 3) '((0 1) (1 2)))
(check-equal? (expand-args '((1 2) 3 )) '((1 2) (3 3)))
(check-equal? (generate-mapped-args '((0 1) (2 3)) '(3 2 1 0)) '((3 2) (1 0)))
(check-equal? (generate-mapped-args '((0) (1)) '(3 2 1 0)) '((3) (2)))

(for ([file (filter (lambda (s) (string-suffix? s ".rkt"))(map path->string (directory-list "tests")))])
    (define file_name (string-append "tests/" file)) 
    (printf "Testing ~a... \n" file_name)
    (check-equal? (run-on-file file_name) (string-trim (file->string (string-append file_name ".qasm")))))