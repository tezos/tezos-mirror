meta:
  id: id_008__ptedo2zk__script
  endian: be
doc: ! 'Encoding id: 008-PtEdo2Zk.script'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  id_008__ptedo2zk__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
seq:
- id: id_008__ptedo2zk__scripted__contracts
  type: id_008__ptedo2zk__scripted__contracts
