meta:
  id: id_006__pscartha__delegate__frozen_balance
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.delegate.frozen_balance'
types:
  id_006__pscartha__mutez:
    seq:
    - id: id_006__pscartha__mutez
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
  type: id_006__pscartha__mutez
- id: fees
  type: id_006__pscartha__mutez
- id: rewards
  type: id_006__pscartha__mutez
