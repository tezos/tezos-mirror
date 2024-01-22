meta:
  id: p2p_address
  endian: be
doc: ! 'Encoding id: p2p_address

  Description: An address for locating peers.'
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
- id: p2p_address
  type: bytes_dyn_uint30
