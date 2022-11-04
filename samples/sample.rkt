(bell 2
  (H 0) 
  (CX 0 1))

(recover 2 
  (CX 0 1) 
  (CZ 1 0))

(circuit 3 
  (bell 0 2) 
  (bell) 
  (measure 0 1) 
  (recover))

(cirq 3
  (circuit 2 1 0)
  (bell))

(run cirq)
