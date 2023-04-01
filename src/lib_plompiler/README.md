Plompiler is a monadic Domain Specific Language embedded in OCaml that
can be used to build circuits for aPlonK.
Programs written in Plompiler are typed to increase safety and can be
compiled to their circuit representations or interpreted directly in
OCaml for testing.
Together with the circuit, Plompiler also returns an efficient
one-pass solver that given an input finds a valid assignment for the
wires of the circuit.

Additionally, Plompiler contains a generic optimizer capable of
significantly reducing the size of commonly developed circuits.
Flamegraphs can be generated to inspect the generated circuits
and guide the optimization of each primitives.

The library also features a highly performant set of primitives for
hashing (Poseidon and Anemoi) and signing.
