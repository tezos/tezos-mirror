meta:
  id: seoul__tez
  endian: be
doc: ! 'Encoding id: seoul.tez'
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
  seoul__mutez:
    seq:
    - id: seoul__mutez
      type: n
seq:
- id: seoul__mutez
  type: seoul__mutez
