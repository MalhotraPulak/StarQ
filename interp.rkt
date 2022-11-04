#lang racket
(require eopl)
(require racket/trace)
(define-datatype circuit-body-item
                 circuit-body-item?
                 [repeat (circuit-name circuit-body-item?) (count number?)]
                 [circuit-call (circuit-name symbol?) (gate-args (listof number?))]
                 ;; [moment (gates (listof circuit-body-item?))]
                 )

(define-datatype
 ast
 ast?
 [run-circuit (circuit-name symbol?)]
 [circuit-def (circuit-name symbol?) (qubit number?) (circuit-body (list-of circuit-body-item?))])

(define (fundamental-gate-name? x)
  (match x
    ['H #t]
    ['S #t]
    ['Z #t]
    ['X #t]
    ['Y #t]
    ['measure #t]
    ['CZ #t]
    ['CX #t]
    [else #f]))

(define (circuit-size circuit-name default-size) 
  (match circuit-name
    ['H 1]
    ['S 1]
    ['Z 1]
    ['X 1]
    ['Y 1]
    ['measure default-size]
    ['CZ 2]
    ['CX 2]
    [else (car (hash-ref submodules circuit-name))]
  )
)

(define (cstCircuitBodyItem->astCircuitBodyItem x)
  (match x
    [(list 'repeat circuit n) (repeat circuit n)]
    [(list name args ...) (circuit-call name args)]
    [(list name) (circuit-call name (list))]
    [else (error "cstGate->astGate: bad input" x)]))

(define (cst->ast x)
  (match x
    [(list 'run name) (run-circuit name)]
    [(list name qubit body ...)
     (circuit-def name qubit (map cstCircuitBodyItem->astCircuitBodyItem body))]
    [else (error "cst->ast: bad input" x)]))

(define submodules (make-hash))
(define (parse-submodules submodule)
  (cases ast
         submodule
         [circuit-def (name qubit body) (hash-set! submodules name (list qubit body))]
         [run-circuit (name) (set! run name)]))

(define (print-gate name args)
  (define indices
    (for/list ([i args])
      (~v i)))
  (define indices-string (string-join indices ","))
  (define final (string-append (symbol->string name) " " "q[" indices-string "]\n"))
  (display final))

(define (generate-qasm submodule arg-mapping)
  (cases circuit-body-item
         submodule
         [repeat
          (circuit n)
          (for ([i (in-range n)])
            (generate-qasm circuit))]
         [circuit-call
          (name args)
          (when (empty? args)
            (set! args (sequence->list (in-range (circuit-size name (length arg-mapping))))))
          (define new_args
            (if (empty? arg-mapping) args (map (lambda (x) (list-ref arg-mapping x)) args)))
          (if (fundamental-gate-name? name)
              (print-gate name new_args)
              (map (lambda (x) (generate-qasm x new_args)) (cadr (hash-ref submodules name))))]))

(define run 'none)
(define cst
  (call-with-input-file "samples.rkt"
                        (lambda (x)
                          (for/list ([e (in-port read x)])
                            e))))
(define z (map parse-submodules (map cst->ast cst)))
(define y (map (lambda (x) (generate-qasm x (list))) (cadr (hash-ref submodules run))))
