meta:
  id: id_019__ptparisa__tez
  endian: be
doc: ! 'Encoding id: 019-PtParisA.tez'
types:
  id_019__ptparisa__mutez:
    seq:
    - id: id_019__ptparisa__mutez
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
- id: id_019__ptparisa__mutez
  type: id_019__ptparisa__mutez
