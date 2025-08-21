meta:
  id: id_023__ptseoulo__tez
  endian: be
doc: ! 'Encoding id: 023-PtSeouLo.tez'
types:
  id_023__ptseoulo__mutez:
    seq:
    - id: id_023__ptseoulo__mutez
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
- id: id_023__ptseoulo__mutez
  type: id_023__ptseoulo__mutez
