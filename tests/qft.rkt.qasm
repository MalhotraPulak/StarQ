version 1.0
qubits 5
H q[0]
CRk q[1], q[0], 2
CRk q[2], q[0], 3
CRk q[3], q[0], 4
CRk q[4], q[0], 5
H q[1]
CRk q[2], q[1], 2
CRk q[3], q[1], 3
CRk q[4], q[1], 4
H q[2]
CRk q[3], q[2], 2
CRk q[4], q[2], 3
H q[3]
CRk q[4], q[3], 2
H q[4]