meta:
  id: sapling__transaction__commitment
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: sapling__transaction__commitment
  type: fixed_bytes
