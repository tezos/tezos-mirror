meta:
  id: id_022__psriotum__tez
  endian: be
doc: ! 'Encoding id: 022-PsRiotum.tez'
types:
  id_022__psriotum__mutez:
    seq:
    - id: id_022__psriotum__mutez
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
- id: id_022__psriotum__mutez
  type: id_022__psriotum__mutez
