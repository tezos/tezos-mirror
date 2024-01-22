meta:
  id: sapling__transaction__output
  endian: be
  imports:
  - sapling__transaction__ciphertext
  - sapling__transaction__commitment
doc: ! 'Encoding id: sapling.transaction.output

  Description: Output of a transaction'
seq:
- id: cm
  type: sapling__transaction__commitment
- id: proof_o
  size: 192
- id: ciphertext
  type: sapling__transaction__ciphertext
