meta:
  id: id_005__psbabym1__delegate__frozen_balance_by_cycles
  endian: be
doc: ! 'Encoding id: 005-PsBabyM1.delegate.frozen_balance_by_cycles'
types:
  id_005__psbabym1__delegate__frozen_balance_by_cycles:
    seq:
    - id: id_005__psbabym1__delegate__frozen_balance_by_cycles_entries
      type: id_005__psbabym1__delegate__frozen_balance_by_cycles_entries
      repeat: eos
  id_005__psbabym1__delegate__frozen_balance_by_cycles_entries:
    seq:
    - id: cycle
      type: s4
    - id: deposit
      type: id_005__psbabym1__mutez
    - id: fees
      type: id_005__psbabym1__mutez
    - id: rewards
      type: id_005__psbabym1__mutez
  id_005__psbabym1__mutez:
    seq:
    - id: id_005__psbabym1__mutez
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
- id: len_id_005__psbabym1__delegate__frozen_balance_by_cycles
  type: s4
- id: id_005__psbabym1__delegate__frozen_balance_by_cycles
  type: id_005__psbabym1__delegate__frozen_balance_by_cycles
  size: len_id_005__psbabym1__delegate__frozen_balance_by_cycles
