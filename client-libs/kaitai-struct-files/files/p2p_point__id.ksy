meta:
  id: p2p_point__id
  endian: be
doc: ! 'Encoding id: p2p_point.id'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  p2p_point__id:
    seq:
    - id: p2p_point__id
      type: bytes_dyn_uint30
seq:
- id: p2p_point__id
  type: p2p_point__id
  doc: Identifier for a peer point
