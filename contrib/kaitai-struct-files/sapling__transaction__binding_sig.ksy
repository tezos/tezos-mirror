meta:
  id: sapling__transaction__binding_sig
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: sapling__transaction__binding_sig
  type: fixed_bytes
