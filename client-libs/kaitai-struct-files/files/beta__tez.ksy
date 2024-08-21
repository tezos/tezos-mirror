meta:
  id: beta__tez
  endian: be
doc: ! 'Encoding id: beta.tez'
types:
  beta__mutez:
    seq:
    - id: beta__mutez
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
- id: beta__mutez
  type: beta__mutez
