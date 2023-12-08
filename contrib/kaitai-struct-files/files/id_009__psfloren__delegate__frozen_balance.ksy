meta:
  id: id_009__psfloren__delegate__frozen_balance
  endian: be
doc: ! 'Encoding id: 009-PsFLoren.delegate.frozen_balance'
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
- id: deposit
  type: id_009__psfloren__mutez
- id: fees
  type: id_009__psfloren__mutez
- id: rewards
  type: id_009__psfloren__mutez
