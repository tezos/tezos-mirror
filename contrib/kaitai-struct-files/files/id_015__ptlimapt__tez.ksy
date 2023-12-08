meta:
  id: id_015__ptlimapt__tez
  endian: be
doc: ! 'Encoding id: 015-PtLimaPt.tez'
types:
  id_015__ptlimapt__mutez:
    seq:
    - id: id_015__ptlimapt__mutez
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
- id: id_015__ptlimapt__mutez
  type: id_015__ptlimapt__mutez
