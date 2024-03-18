meta:
  id: alpha__tez
  endian: be
doc: ! 'Encoding id: alpha.tez'
types:
  alpha__mutez:
    seq:
    - id: alpha__mutez
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
- id: alpha__mutez
  type: alpha__mutez
