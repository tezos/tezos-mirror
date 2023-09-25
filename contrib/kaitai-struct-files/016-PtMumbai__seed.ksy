meta:
  id: 016-PtMumbai__seed
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: fixed size (uint30) bytes
  type: fixed_bytes
