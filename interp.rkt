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

(define (circuit-size circuit-name default-size submodules-info)
  (match circuit-name
    ['H 1]
    ['S 1]
    ['Z 1]
    ['X 1]
    ['Y 1]
    ['measure default-size]
    ['CZ 2]
    ['CX 2]
    [else (car (hash-ref submodules-info circuit-name))]))

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

(define (print-gate name args)
  (define indices
    (for/list ([i args])
      (~v i)))
  (define indices-string (string-join indices ","))
  (define final (string-append (symbol->string name) " " "q[" indices-string "]\n"))
  final)

(define (expand-args args)
  (flatten (for/list ([i args])
             (if (list? i) (range (car i) (add1 (cadr i))) i))))

(define (strings->string strs)
  (apply string-append strs))

(define (generate-qasm-from-circuit-item submodules-info circuit-item arg-mapping)
  (cases circuit-body-item
         circuit-item
         [repeat
          (circuit n)
          (for ([i (range n)])
            (generate-qasm-from-circuit-item circuit))]
         [circuit-call
          (name args)
          (when (empty? args)
            (set! args (range (circuit-size name (length arg-mapping) submodules-info))))
          (define new_args
            (if (empty? arg-mapping) args (map (lambda (x) (list-ref arg-mapping x)) args)))
          (if (fundamental-gate-name? name)
              (print-gate name new_args)
              (generate-qasm-from-name submodules-info name new_args))]))

(define (generate-qasm-from-name submodules-info submodule-name arg-mapping)
  (define submodules (cdr (hash-ref submodules-info submodule-name)))
  (define qasms (map (lambda (circuit-item)
         (generate-qasm-from-circuit-item submodules-info circuit-item arg-mapping))
       submodules))
  (strings->string qasms))

(define (parse-submodule submodules-info)
  (lambda (submodule)
    (cases ast
           submodule
           [circuit-def (name qubit body) (hash-set! submodules-info name (cons qubit body))]
           [run-circuit (name) (hash-set! submodules-info 'run name)])))

(define (run-on-file filename)
  (define cst
    (call-with-input-file filename
                          (lambda (x)
                            (for/list ([e (in-port read x)])
                              e))))
  (define ast (map cst->ast cst))
  (define submodules-info (make-hash))
  (map (parse-submodule submodules-info) ast)
  (define run-name (hash-ref submodules-info 'run))
  (printf "Generating circuit for ~a\n" run-name)
  (define qasm (generate-qasm-from-name submodules-info run-name (list)))
  (printf qasm)
  0)

(run-on-file "samples/sample.rkt")
