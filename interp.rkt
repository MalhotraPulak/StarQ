#lang racket
(require eopl)
(require racket/trace)
(define-datatype circuit-body-item
                 circuit-body-item?
                 [repeat (circuit circuit-body-item?) (count number?)]
                 [circuit-call (circuit-name symbol?) (gate-args list?)]
                 [moment (circuits (list-of circuit-body-item?))])

(define-datatype
 ast
 ast?
 [run-circuit (circuit-name symbol?)]
 [circuit-def (circuit-name symbol?) (qubit number?) (circuit-body (list-of circuit-body-item?))])

(define-datatype qasm qasm? [instr (name symbol?) (args (list-of number?))])

(define (fundamental-gate? name)
  (match name
    ['H 1]
    ['S 1]
    ['Z 1]
    ['X 1]
    ['Y 1]
    ['measure_z 1]
    ['CZ 2]
    ['CX 2]
    [else #f]))

(define (circuit-size circuit-name submodules-info)
  (if (fundamental-gate? circuit-name)
      (fundamental-gate? circuit-name)
      (car (hash-ref submodules-info circuit-name))))

(define (range-expand args)
  (match args
    [(? number?) args]
    [(list x ': y) (range x (add1 y))]
    [(? list?) (map range-expand args)]
    [else (error "range-expand: bad args" args)]))

(define (cstCircuitBodyItem->astCircuitBodyItem x)
  (match x
    [(list 'repeat circuit n) (repeat (cstCircuitBodyItem->astCircuitBodyItem circuit) n)]
    [(list name args ...)
     #:when (symbol? name)
     (circuit-call name (range-expand args))]
    [(list name)
     #:when (symbol? name)
     (circuit-call name (list))]
    [(list circuits ...) (moment (map cstCircuitBodyItem->astCircuitBodyItem circuits))]
    [else (error "cstGate->astGate: bad input" x)]))

(define (cst->ast x)
  (match x
    [(list 'run name) (run-circuit name)]
    [(list name qubit body ...)
     (circuit-def name qubit (map cstCircuitBodyItem->astCircuitBodyItem body))]
    [else (error "cst->ast: bad input" x)]))

(define (print-one-gate name args)
  (list (instr name args)))

(define (expand-args args)
  (define max-len 1)
  (for ([lst args])
    (when (list? lst)
      (set! max-len (max max-len (length lst)))))
  (for/list ([lst args])
    (cond
      [(and (list? lst) (eq? (length lst) max-len)) lst]
      [(number? lst) (make-list max-len lst)])))

(define (strings->string strs)
  (apply string-append strs))

(define (generate-args sz max-qubits)
  (for/list ([i (range sz)])
    (range i (+ i (add1 (- max-qubits sz))))))

(define (generate-mapped-args args arg-mapping)
  (match args
    [(? list?) (map (lambda (arg) (generate-mapped-args arg arg-mapping)) args)]
    [(? number?) (list-ref arg-mapping args)]))

(define (qasm->string instruction)
              (cases qasm
                     instruction
                     [instr
                      (name args)
                      (define indices
                        (for/list ([i args])
                          (format "q[~a]" i)))
                      (define indices-string (string-join indices ", "))
                      (define final (string-append (symbol->string name) " " indices-string))
                      final]))

(define (qasms->string qasms)
  (define instrs (map qasm->string qasms))
  (string-join instrs "\n"))

(define (generate-qasm-from-circuit-item submodules-info circuit-item arg-mapping max-qubits)
  (cases circuit-body-item
         circuit-item
         [repeat
          (circuit n)
          (define qasms
            (for/list ([i (range n)])
              (generate-qasm-from-circuit-item submodules-info circuit arg-mapping max-qubits)))
          (flatten qasms)]
         [moment
          (circuits)
          (define qasms
            (for/list ([circuit circuits])
              (generate-qasm-from-circuit-item submodules-info circuit arg-mapping max-qubits)))
          (flatten qasms)]
         [circuit-call
          (name args)
          (define sz (circuit-size name submodules-info))
          (when (empty? args)
            (set! args (generate-args sz max-qubits)))
          (define multi-args (expand-args args))
          (define instances (length (car multi-args)))
          (define qasms
            (for/list ([i (in-range instances)])
              (define args (map car multi-args))
              (set! multi-args (map cdr multi-args))
              (set! args (if (empty? arg-mapping) args (generate-mapped-args args arg-mapping)))
              (if (fundamental-gate? name)
                  (print-one-gate name args)
                  (generate-qasm-from-name submodules-info name args))))
          (flatten qasms)]))

(define (generate-qasm-from-name submodules-info submodule-name arg-mapping)
  (define max-qubits (car (hash-ref submodules-info submodule-name)))
  (define submodules (cdr (hash-ref submodules-info submodule-name)))
  (define qasms
    (map (lambda (circuit-item)
           (generate-qasm-from-circuit-item submodules-info circuit-item arg-mapping max-qubits))
         submodules))
  (flatten qasms))

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
  (define qasms (generate-qasm-from-name submodules-info run-name (list)))
  (qasms->string qasms))

(provide (all-defined-out))
