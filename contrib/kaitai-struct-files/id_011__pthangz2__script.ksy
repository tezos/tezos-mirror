meta:
  id: id_011__pthangz2__script
  endian: be
doc: ! 'Encoding id: 011-PtHangz2.script'
types:
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  id_011__pthangz2__scripted__contracts:
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
- id: id_011__pthangz2__scripted__contracts
  type: id_011__pthangz2__scripted__contracts
