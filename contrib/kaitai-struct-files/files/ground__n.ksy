meta:
  id: ground__n
  endian: be
doc: ! 'Encoding id: ground.N

  Description: Arbitrary precision natural numbers'
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
- id: ground__n
  type: n
