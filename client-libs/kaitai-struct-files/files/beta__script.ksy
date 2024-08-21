meta:
  id: beta__script
  endian: be
doc: ! 'Encoding id: beta.script'
types:
  beta__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
seq:
- id: beta__scripted__contracts
  type: beta__scripted__contracts
