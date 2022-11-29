# StarQ: A Minimal and Accessible Quantum Circuit Representation DSL

### To Run
To get cQASM for a given StarQ file
```bash
racket cli.rkt --file <filename>
```
We have a suite of 25 different tests which cover all the functionality of the language. <br/>
To run tests:
```bash
raco test tester.rkt
```
<br/>
We have provided code samples along with expected outputs for `Grover Search`, `Quantum Fourier Transform`, `Quantum Teleportation` and `Quantum Classification` in the tests folder itself.

### Interpreter Design

We followed Test Driven Development and have a suite of 25 tests.

Our interpreter is a transpiler that evaluates to cQasm. We have extensively used `eopl` data structures and recursive descent parsing to write succinct and concise code.

AST datatype:
```racket
(define-datatype
 ast
 ast?
 [run-circuit (circuit-name symbol?)]
 [circuit-def (circuit-name symbol?) (qubit number?) (circuit-body (list-of circuit-body-item?))])

(define-datatype circuit-body-item
                 circuit-body-item?
                 [chops (circuit circuit-body-item?) (count number?)]
                 [shift (circuit circuit-body-item?) (count number?)]
                 [repeat (circuit circuit-body-item?) (count number?)]
                 [uncompute (circuit circuit-body-item?)]
                 [circuit-call (circuit-name symbol?) (gate-args list?)]
                 [moment (circuits (list-of circuit-body-item?))])
```
Once we convert AST into cQASM we store it in a qasm datatype:
```racket
(define-datatype qasm
                 qasm?
                 [instr (name symbol?) (constant false-or-number?) (args (list-of number?))]
                 [instr-parallel (instrs (list-of qasm?))]
                 [instr-label (label symbol?) (count number?)])
```
We maintain all the inbuilt gates and their properties in an encapsulated match function:
Each entry corresponds to `(arity constants uncompute)`
```
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
    ;; invalid uncomputes
    ['measure_z '(1 #f #f)]
    ['measure_y '(1 #f #f)]
    ['measure_x '(1 #f #f)]
    [else #f]))
```

