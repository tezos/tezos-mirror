meta:
  id: sapling__transaction__input
  endian: be
doc: ! 'Encoding id: sapling.transaction.input

  Description: Input of a transaction'
seq:
- id: cv
  size: 32
- id: nf
  size: 32
- id: rk
  size: 32
- id: proof_i
  size: 192
- id: signature
  size: 64
