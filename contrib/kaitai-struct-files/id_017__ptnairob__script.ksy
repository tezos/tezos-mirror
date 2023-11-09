meta:
  id: id_017__ptnairob__script
  endian: be
doc: ! 'Encoding id: 017-PtNairob.script'
types:
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  id_017__ptnairob__scripted__contracts:
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
- id: id_017__ptnairob__scripted__contracts
  type: id_017__ptnairob__scripted__contracts
