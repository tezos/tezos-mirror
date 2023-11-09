meta:
  id: id_009__psfloren__tez
  endian: be
doc: ! 'Encoding id: 009-PsFLoren.tez'
types:
  id_009__psfloren__mutez:
    seq:
    - id: id_009__psfloren__mutez
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
- id: id_009__psfloren__mutez
  type: id_009__psfloren__mutez
