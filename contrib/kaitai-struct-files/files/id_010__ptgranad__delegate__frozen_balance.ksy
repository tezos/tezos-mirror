meta:
  id: id_010__ptgranad__delegate__frozen_balance
  endian: be
doc: ! 'Encoding id: 010-PtGRANAD.delegate.frozen_balance'
types:
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
- id: deposits
  type: id_010__ptgranad__mutez
- id: fees
  type: id_010__ptgranad__mutez
- id: rewards
  type: id_010__ptgranad__mutez
