meta:
  id: id_007__psdelph1__script
  endian: be
doc: ! 'Encoding id: 007-PsDELPH1.script'
types:
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  id_007__psdelph1__scripted__contracts:
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
- id: id_007__psdelph1__scripted__contracts
  type: id_007__psdelph1__scripted__contracts
