(top-circuit 5
	(H 0)
	(CRk 2 1 0)
	(CRk 3 2 0)
	(CRk 4 3 0)
	(CRk 5 4 0))

(fourier 5
	(top-circuit)
	(-> (% (top-circuit) 1) 1)
	(-> (% (top-circuit) 2) 2)
	(-> (% (top-circuit) 3) 3)
	(-> (% (top-circuit) 4) 4))