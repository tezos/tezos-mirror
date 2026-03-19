meta:
  id: id_025__psgxqmsv__tez
  endian: be
doc: ! 'Encoding id: 025-PsGXQmSv.tez'
types:
  id_025__psgxqmsv__mutez:
    seq:
    - id: id_025__psgxqmsv__mutez
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
- id: id_025__psgxqmsv__mutez
  type: id_025__psgxqmsv__mutez
