meta:
  id: id_021__ptqenab1__tez
  endian: be
doc: ! 'Encoding id: 021-PtQenaB1.tez'
types:
  id_021__ptqenab1__mutez:
    seq:
    - id: id_021__ptqenab1__mutez
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
- id: id_021__ptqenab1__mutez
  type: id_021__ptqenab1__mutez
