meta:
  id: alpha__script
  endian: be
doc: ! 'Encoding id: alpha.script'
types:
  alpha__scripted__contracts:
    seq:
    - id: code
      type: code
    - id: storage
      type: storage
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
seq:
- id: alpha__scripted__contracts
  type: alpha__scripted__contracts
