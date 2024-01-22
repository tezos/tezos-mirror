meta:
  id: id_012__psithaca__tez
  endian: be
doc: ! 'Encoding id: 012-Psithaca.tez'
types:
  id_012__psithaca__mutez:
    seq:
    - id: id_012__psithaca__mutez
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
- id: id_012__psithaca__mutez
  type: id_012__psithaca__mutez
