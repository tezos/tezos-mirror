aPlonK is a PlonK-inspired proving system which focuses
on proof-aggregation and distributed proof generation.
It is an OCaml implementation from scratch of the PlonK proving
system based on the bls12-381 curve, which eases the integration
of new custom gates and lookup tables.
Instead of the classic KZG commitment scheme, our custom aggregation
protocol can verify multiple proofs in logarithmic time.