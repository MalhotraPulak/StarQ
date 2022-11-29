version 1.0
qubits 3
{ H q[0] | H q[1] | H q[2] }
.shift(2)
X q[0]
H q[2]
Toffoli q[0], q[1], q[2]
H q[2]
X q[0]
{ H q[0] | H q[1] | H q[2] }
{ X q[0] | X q[1] | X q[2] }
H q[2]
Toffoli q[0], q[1], q[2]
H q[2]
{ X q[0] | X q[1] | X q[2] }
{ H q[0] | H q[1] | H q[2] }
.end