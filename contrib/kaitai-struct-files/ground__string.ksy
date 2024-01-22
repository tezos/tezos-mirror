meta:
  id: ground__string
  endian: be
doc: ! 'Encoding id: ground.string'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
seq:
- id: ground__string
  type: bytes_dyn_uint30
