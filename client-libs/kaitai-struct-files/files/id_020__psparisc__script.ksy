meta:
  id: id_020__psparisc__script
  endian: be
doc: ! 'Encoding id: 020-PsParisC.script'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  id_020__psparisc__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
seq:
- id: id_020__psparisc__scripted__contracts
  type: id_020__psparisc__scripted__contracts
