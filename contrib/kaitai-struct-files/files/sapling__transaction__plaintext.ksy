meta:
  id: sapling__transaction__plaintext
  endian: be
  imports:
  - sapling__transaction__rcm
doc: ! 'Encoding id: sapling.transaction.plaintext'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  sapling__wallet__diversifier:
    seq:
    - id: sapling__wallet__diversifier
      size: 11
seq:
- id: diversifier
  type: sapling__wallet__diversifier
- id: amount
  type: s8
- id: rcm
  type: sapling__transaction__rcm
- id: memo
  type: bytes_dyn_uint30
