(oracle 3 [(X 0) (H 2)] (Toffoli) [(H 2) (X 0)])
(reflection 3 (H) (X) (H 2))
(diffusion 3 (reflection) (Toffoli) (uncompute (reflection)))
(grover 3 (H) (oracle) (diffusion))
