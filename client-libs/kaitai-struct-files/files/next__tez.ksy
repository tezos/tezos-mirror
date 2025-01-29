meta:
  id: next__tez
  endian: be
doc: ! 'Encoding id: next.tez'
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
  next__mutez:
    seq:
    - id: next__mutez
      type: n
seq:
- id: next__mutez
  type: next__mutez
