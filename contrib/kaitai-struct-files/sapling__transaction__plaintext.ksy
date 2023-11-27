meta:
  id: sapling__transaction__plaintext
  endian: be
  imports:
  - sapling__transaction__rcm
doc: ! 'Encoding id: sapling.transaction.plaintext'
types:
  memo:
    seq:
    - id: len_memo
      type: s4
    - id: memo
      size: len_memo
seq:
- id: diversifier
  size: 11
- id: amount
  type: s8
- id: rcm
  type: sapling__transaction__rcm
- id: memo
  type: memo
