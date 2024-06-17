meta:
  id: id_020__psparisc__tez
  endian: be
doc: ! 'Encoding id: 020-PsParisC.tez'
types:
  id_020__psparisc__mutez:
    seq:
    - id: id_020__psparisc__mutez
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
- id: id_020__psparisc__mutez
  type: id_020__psparisc__mutez
