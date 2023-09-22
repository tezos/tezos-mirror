meta:
  id: ground__variable__bytes
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: ground__variable__bytes
  type: fixed_bytes
