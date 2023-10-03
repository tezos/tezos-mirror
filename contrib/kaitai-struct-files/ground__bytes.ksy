meta:
  id: ground__bytes
  endian: be
types:
  ground__bytes:
    meta:
      id: ground__bytes
      endian: be
    seq:
    - id: ground__bytes
      size-eos: true
seq:
- id: len_ground__bytes
  type: s4
- id: ground__bytes
  type: ground__bytes
  size: len_ground__bytes
