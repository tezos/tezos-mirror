meta:
  id: id_018__proxford__tez
  endian: be
doc: ! 'Encoding id: 018-Proxford.tez'
types:
  id_018__proxford__mutez:
    seq:
    - id: id_018__proxford__mutez
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
- id: id_018__proxford__mutez
  type: id_018__proxford__mutez
