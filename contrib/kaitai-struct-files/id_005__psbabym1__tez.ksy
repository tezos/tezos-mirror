meta:
  id: id_005__psbabym1__tez
  endian: be
doc: ! 'Encoding id: 005-PsBabyM1.tez'
types:
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
- id: id_005__psbabym1__mutez
  type: n
