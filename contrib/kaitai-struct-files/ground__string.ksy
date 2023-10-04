meta:
  id: ground__string
  endian: be
types:
  ground__string:
    meta:
      id: ground__string
      endian: be
    seq:
    - id: ground__string
      type: variable size bytes
seq:
- id: len_ground__string
  type: s4
- id: ground__string
  type: ground__string
  size: len_ground__string
