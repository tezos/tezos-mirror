meta:
  id: id_014__ptkathma__tez
  endian: be
doc: ! 'Encoding id: 014-PtKathma.tez'
types:
  id_014__ptkathma__mutez:
    seq:
    - id: id_014__ptkathma__mutez
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
- id: id_014__ptkathma__mutez
  type: id_014__ptkathma__mutez
