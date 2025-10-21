meta:
  id: id_024__psd5wvtj__tez
  endian: be
doc: ! 'Encoding id: 024-PsD5wVTJ.tez'
types:
  id_024__psd5wvtj__mutez:
    seq:
    - id: id_024__psd5wvtj__mutez
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
- id: id_024__psd5wvtj__mutez
  type: id_024__psd5wvtj__mutez
