meta:
  id: seoul__script
  endian: be
doc: ! 'Encoding id: seoul.script'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  seoul__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
seq:
- id: seoul__scripted__contracts
  type: seoul__scripted__contracts
