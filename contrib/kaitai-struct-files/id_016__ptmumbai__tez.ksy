meta:
  id: id_016__ptmumbai__tez
  endian: be
doc: ! 'Encoding id: 016-PtMumbai.tez'
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
- id: id_016__ptmumbai__mutez
  type: n
