meta:
  id: id_006__pscartha__delegate__frozen_balance_by_cycles
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.delegate.frozen_balance_by_cycles'
types:
  id_006__pscartha__delegate__frozen_balance_by_cycles:
    seq:
    - id: id_006__pscartha__delegate__frozen_balance_by_cycles_entries
      type: id_006__pscartha__delegate__frozen_balance_by_cycles_entries
      repeat: eos
  id_006__pscartha__delegate__frozen_balance_by_cycles_entries:
    seq:
    - id: cycle
      type: s4
    - id: deposit
      type: id_006__pscartha__mutez
    - id: fees
      type: id_006__pscartha__mutez
    - id: rewards
      type: id_006__pscartha__mutez
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
- id: len_id_006__pscartha__delegate__frozen_balance_by_cycles
  type: s4
- id: id_006__pscartha__delegate__frozen_balance_by_cycles
  type: id_006__pscartha__delegate__frozen_balance_by_cycles
  size: len_id_006__pscartha__delegate__frozen_balance_by_cycles
