meta:
  id: id_024__pttallin__tez
  endian: be
doc: ! 'Encoding id: 024-PtTALLiN.tez'
types:
  id_024__pttallin__mutez:
    seq:
    - id: id_024__pttallin__mutez
      type: n
  n:
    seq:
    - id: n
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
seq:
- id: id_024__pttallin__mutez
  type: id_024__pttallin__mutez
