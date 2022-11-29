(recover 3 (CNOT 0 2) (CZ 1 2))
(bell 2 (H 0) (CNOT))

(teleportation 3 
    (bell 0 2) 
    (! (bell 1 0)) 
    (measure (0 1)) 
    (recover))