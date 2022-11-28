#lang racket
(require eopl)
(require racket/trace)
(define-datatype circuit-body-item
                 circuit-body-item?
                 [repeat (circuit circuit-body-item?) (count number?)]
                 [uncompute (circuit circuit-body-item?)]
                 [circuit-call (circuit-name symbol?) (gate-args list?)]
                 [moment (circuits (list-of circuit-body-item?))])

(define-datatype
 ast
 ast?
 [run-circuit (circuit-name symbol?)]
 [circuit-def (circuit-name symbol?) (qubit number?) (circuit-body (list-of circuit-body-item?))])

(define false-or-number?
  (or/c boolean? number?))
(define-datatype qasm qasm? [instr (name symbol?) (constant false-or-number?) (args (list-of number?))])
(define special-uncompute (list 'Rxdag 'Rydag 'Rzdag 'CRdag 'CRkdag))
(define (fundamental-gate? name)
  (match name
    ['X '(1 #f X)]
    ['Y '(1 #f Y)]
    ['Z '(1 #f Z)]
    ['H '(1 #f H)]
    ['I '(1 #f I)]
    ['S '(1 #f Sdag)]
    ['Sdag '(1 #f S)]
    ['T '(1 #f Tdag)]
    ['Tdag '(1 #f T)]
    ['CNOT '(2 #f CNOT)]
    ['SWAP '(2 #f SWAP)]
    ['CZ '(2 #f CZ)]
    ['CX '(2 #f CX)]
    ;; special uncompute
    ['Rx '(1 1 Rxdag)]
    ['Ry '(1 1 Rydag)]
    ['Rz '(1 1 Rzdag)]
    ['CR '(2 1 CRdag)]
    ['CRk '(2 1 CRkdag)]
    ;; special uncompute ends
    ['Toffoli '(3 #f Toffoli)]
    ['measure_z '(1 #f #f)]
    ['measure_y '(1 #f #f)]
    ['measure_x '(1 #f #f)]
    ;; uncompute hack
    ['Rxdag '(1 1 Rxdag)]
    ['Rydag '(1 1 Rydag)]
    ['Rzdag '(1 1 Rzdag)]
    ['CRdag '(2 1 CRdag)]
    ['CRkdag '(2 1 CRkdag)]
    [else #f]))

(define (constant-args name)
  (if (fundamental-gate? name)
    (second (fundamental-gate? name))
    #f))

(define (uncompute-gate name)
  (third (fundamental-gate? name)))

(define (circuit-size circuit-name submodules-info)
  (if (fundamental-gate? circuit-name)
      (first (fundamental-gate? circuit-name))
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
    [(list 'uncompute circuit) (uncompute (cstCircuitBodyItem->astCircuitBodyItem circuit))]
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

(define (print-one-gate name constant args)
  (unless (eq? (length args) (circuit-size name '()))
    (error "print-one-gate: wrong number of args ~a for gate ~a" (length args) name))
  ;; if name in special uncompute gate
  (when (member name special-uncompute)
      (begin 
      (set! name (uncompute-gate name))
      (set! constant (- constant)))) ;; reverse angle for uncompute gates
  (list (instr name constant args)))

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
          (name constant args)
          (define indices
            (for/list ([i args])
              (format "q[~a]" i)))
          (when constant
            (set! indices (append indices (list (format "~a" constant)))))
          (define indices-string (string-join indices ", "))
          (define final (string-append (symbol->string name) " " indices-string))
          final]))

(define (qasms->string qasms)
  (define instrs (map qasm->string qasms))
  (string-join instrs "\n"))

(define (check-moment qasms)
  (define seen (make-hash))
  (for ([instruction qasms])
    (cases qasm
           instruction
           [instr
            (name constant args)
            (for ([arg args])
              (when (hash-ref seen arg #f)
                (error "check-moment: qubit used twice"))
              (hash-set! seen arg #t))]))
  qasms)

(define (generate-qasm-from-circuit-item submodules-info circuit-item arg-mapping max-qubits uncompute?)
  (cases circuit-body-item
         circuit-item
         [uncompute
          (circuit)
          (define qasms
            (generate-qasm-from-circuit-item submodules-info
                                             circuit
                                             arg-mapping
                                             max-qubits
                                             (not uncompute?)
                                             ))
          (flatten qasms)]
         [repeat
          (circuit n)
          (define qasms
            (for/list ([i (range n)])
              (generate-qasm-from-circuit-item submodules-info circuit arg-mapping max-qubits uncompute?)))
          (flatten qasms)]
         [moment
          (circuits)
          (define qasms
            (for/list ([circuit circuits])
              (generate-qasm-from-circuit-item submodules-info circuit arg-mapping max-qubits uncompute?)))
          (check-moment (flatten qasms))]
         [circuit-call
          (name args)
          (define old-name name)
          (define sz (circuit-size name submodules-info))
          (when (empty? args)
            (set! args (generate-args sz max-qubits)))
          (define has-constant? (constant-args name))
          (define constant #f)
          (when has-constant?
            (set! constant (first args))
            (set! args (rest args)))
          (define multi-args (expand-args args))
          (define instances (length (car multi-args)))
          (define qasms
            (for/list ([i (in-range instances)])
              (define args (map car multi-args))
              (set! multi-args (map cdr multi-args))
              (set! args (if (empty? arg-mapping) args (generate-mapped-args args arg-mapping)))
              (if (fundamental-gate? name)
                  (begin
                  (when uncompute? (set! name (uncompute-gate name)))
                  (when (eq? name #f) (error "Uncompute on invalid gate ~a" old-name)) ;; add test
                  (print-one-gate name constant args))
                  (generate-qasm-from-name submodules-info name args uncompute?))))
          (flatten qasms)]))

(define (generate-qasm-from-name submodules-info submodule-name arg-mapping uncompute?)
  (define max-qubits (car (hash-ref submodules-info submodule-name)))
  (define submodules (cdr (hash-ref submodules-info submodule-name)))
  (when uncompute?
    (set! submodules (reverse submodules)))
  (define qasms
    (map (lambda (circuit-item)
           (generate-qasm-from-circuit-item submodules-info circuit-item arg-mapping max-qubits uncompute?))
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
  (define qasms (generate-qasm-from-name submodules-info run-name (list) #f))
  (qasms->string qasms))

(provide (all-defined-out))
