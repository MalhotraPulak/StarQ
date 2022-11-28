(encode 2
    (CNOT 0 1) (Ry 0.1105 1) (CNOT 0 1) (Ry -0.1105 1) (X 0))
(zero 3
    (CNOT 0 2)
    (Ry 0 2))
(phi/4 3
    (CNOT 0 2)
    (Ry -1.511125 2))
(-phi/4 3
    (CNOT 0 2)
    (Ry 1.511125 2))
(first 3
    (Toffoli) (zero) (zero) (Toffoli) (zero) (zero) (X 0))
(second 3
    (Toffoli) (phi/4) (-phi/4) (Toffoli) (-phi/4) (phi/4))
(classification 4
    (H (0 1))
    (encode 1 2)
    (first 0 1 2)
    (second 0 1 2)
    (CNOT 0 3)
    (H 1)
    (measure_z (1 3)))