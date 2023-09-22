meta:
  id: alpha__seed
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: alpha__seed
  type: fixed_bytes
