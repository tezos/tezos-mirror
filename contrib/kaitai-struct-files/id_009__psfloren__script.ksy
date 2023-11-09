meta:
  id: id_009__psfloren__script
  endian: be
doc: ! 'Encoding id: 009-PsFLoren.script'
types:
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  id_009__psfloren__scripted__contracts:
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
- id: id_009__psfloren__scripted__contracts
  type: id_009__psfloren__scripted__contracts
