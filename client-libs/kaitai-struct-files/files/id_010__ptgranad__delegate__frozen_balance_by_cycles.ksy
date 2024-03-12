meta:
  id: id_010__ptgranad__delegate__frozen_balance_by_cycles
  endian: be
doc: ! 'Encoding id: 010-PtGRANAD.delegate.frozen_balance_by_cycles'
types:
  id_010__ptgranad__delegate__frozen_balance_by_cycles:
    seq:
    - id: id_010__ptgranad__delegate__frozen_balance_by_cycles_entries
      type: id_010__ptgranad__delegate__frozen_balance_by_cycles_entries
      repeat: eos
  id_010__ptgranad__delegate__frozen_balance_by_cycles_entries:
    seq:
    - id: cycle
      type: s4
    - id: deposits
      type: id_010__ptgranad__mutez
    - id: fees
      type: id_010__ptgranad__mutez
    - id: rewards
      type: id_010__ptgranad__mutez
  id_010__ptgranad__mutez:
    seq:
    - id: id_010__ptgranad__mutez
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
- id: len_id_010__ptgranad__delegate__frozen_balance_by_cycles
  type: u4
  valid:
    max: 1073741823
- id: id_010__ptgranad__delegate__frozen_balance_by_cycles
  type: id_010__ptgranad__delegate__frozen_balance_by_cycles
  size: len_id_010__ptgranad__delegate__frozen_balance_by_cycles
