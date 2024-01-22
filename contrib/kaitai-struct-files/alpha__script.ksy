meta:
  id: alpha__script
  endian: be
doc: ! 'Encoding id: alpha.script'
types:
  alpha__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
seq:
- id: alpha__scripted__contracts
  type: alpha__scripted__contracts
