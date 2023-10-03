meta:
  id: ground__string
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: ground__string
  type: fixed_bytes
