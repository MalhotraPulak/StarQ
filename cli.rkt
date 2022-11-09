;#! usr/bin/env racket
#lang racket
(require "interp.rkt")
(define path (make-parameter #false))

;; command line parser
(define parser
  (command-line #:usage-help "Have the computer greet you!"
                #:once-each [("-f" "--file") PATH "Filepath to convert to cQasm" (path PATH)]
                #:args ()
                (void)))

(define (get-path f)
  (cond
    [(boolean? f) "Use --file argument"]
    [(string? f) (run-on-file f)]))

;; prints result to the command line
(printf "~a\n" (get-path (path)))
