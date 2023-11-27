meta:
  id: sapling__transaction__input
  endian: be
  imports:
  - sapling__transaction__commitment_value
  - sapling__transaction__nullifier
doc: ! 'Encoding id: sapling.transaction.input

  Description: Input of a transaction'
seq:
- id: cv
  type: sapling__transaction__commitment_value
- id: nf
  type: sapling__transaction__nullifier
- id: rk
  size: 32
- id: proof_i
  size: 192
- id: signature
  size: 64
