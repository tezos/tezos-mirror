meta:
  id: id_013__ptjakart__tez
  endian: be
doc: ! 'Encoding id: 013-PtJakart.tez'
types:
  id_013__ptjakart__mutez:
    seq:
    - id: id_013__ptjakart__mutez
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
- id: id_013__ptjakart__mutez
  type: id_013__ptjakart__mutez
