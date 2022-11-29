version 1.0
qubits 4
H q[0]
H q[1]
CNOT q[1], q[2]
Ry q[2], 0.1105
CNOT q[1], q[2]
Ry q[2], -0.1105
X q[1]
Toffoli q[0], q[1], q[2]
CNOT q[0], q[2]
Ry q[2], 0
CNOT q[0], q[2]
Ry q[2], 0
Toffoli q[0], q[1], q[2]
CNOT q[0], q[2]
Ry q[2], 0
CNOT q[0], q[2]
Ry q[2], 0
X q[0]
Toffoli q[0], q[1], q[2]
CNOT q[0], q[2]
Ry q[2], -1.511125
CNOT q[0], q[2]
Ry q[2], 1.511125
Toffoli q[0], q[1], q[2]
CNOT q[0], q[2]
Ry q[2], 1.511125
CNOT q[0], q[2]
Ry q[2], -1.511125
CNOT q[0], q[3]
H q[1]
measure_z q[1]
measure_z q[3]