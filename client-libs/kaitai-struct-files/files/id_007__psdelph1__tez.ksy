meta:
  id: id_007__psdelph1__tez
  endian: be
doc: ! 'Encoding id: 007-PsDELPH1.tez'
types:
  id_007__psdelph1__mutez:
    seq:
    - id: id_007__psdelph1__mutez
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
- id: id_007__psdelph1__mutez
  type: id_007__psdelph1__mutez
