meta:
  id: id_008__ptedo2zk__delegate__frozen_balance_by_cycles
  endian: be
doc: ! 'Encoding id: 008-PtEdo2Zk.delegate.frozen_balance_by_cycles'
types:
  id_008__ptedo2zk__delegate__frozen_balance_by_cycles_entries:
    seq:
    - id: cycle
      type: s4
    - id: deposit
      type: n
    - id: fees
      type: n
    - id: rewards
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
- id: len_id_008__ptedo2zk__delegate__frozen_balance_by_cycles
  type: s4
- id: id_008__ptedo2zk__delegate__frozen_balance_by_cycles
  type: id_008__ptedo2zk__delegate__frozen_balance_by_cycles_entries
  size: len_id_008__ptedo2zk__delegate__frozen_balance_by_cycles
  repeat: eos
