meta:
  id: id_007__psdelph1__delegate__frozen_balance
  endian: be
doc: ! 'Encoding id: 007-PsDELPH1.delegate.frozen_balance'
types:
  id_007__psdelph1__mutez:
    seq:
    - id: id_007__psdelph1__mutez
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
  type: id_007__psdelph1__mutez
- id: fees
  type: id_007__psdelph1__mutez
- id: rewards
  type: id_007__psdelph1__mutez
