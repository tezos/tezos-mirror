meta:
  id: id_006__pscartha__script
  endian: be
types:
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  id_006__pscartha__scripted__contracts:
    seq:
    - id: code
      type: code
    - id: storage
      type: storage
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
seq:
- id: id_006__pscartha__scripted__contracts
  type: id_006__pscartha__scripted__contracts
