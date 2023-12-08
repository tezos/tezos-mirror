meta:
  id: id_013__ptjakart__script
  endian: be
doc: ! 'Encoding id: 013-PtJakart.script'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  id_013__ptjakart__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
seq:
- id: id_013__ptjakart__scripted__contracts
  type: id_013__ptjakart__scripted__contracts
