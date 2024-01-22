meta:
  id: id_007__psdelph1__delegate__frozen_balance_by_cycles
  endian: be
doc: ! 'Encoding id: 007-PsDELPH1.delegate.frozen_balance_by_cycles'
types:
  id_007__psdelph1__delegate__frozen_balance_by_cycles:
    seq:
    - id: id_007__psdelph1__delegate__frozen_balance_by_cycles_entries
      type: id_007__psdelph1__delegate__frozen_balance_by_cycles_entries
      repeat: eos
  id_007__psdelph1__delegate__frozen_balance_by_cycles_entries:
    seq:
    - id: cycle
      type: s4
    - id: deposit
      type: id_007__psdelph1__mutez
    - id: fees
      type: id_007__psdelph1__mutez
    - id: rewards
      type: id_007__psdelph1__mutez
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
- id: len_id_007__psdelph1__delegate__frozen_balance_by_cycles
  type: u4
  valid:
    max: 1073741823
- id: id_007__psdelph1__delegate__frozen_balance_by_cycles
  type: id_007__psdelph1__delegate__frozen_balance_by_cycles
  size: len_id_007__psdelph1__delegate__frozen_balance_by_cycles
