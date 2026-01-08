meta:
  id: alpha__script
  endian: be
doc: ! 'Encoding id: alpha.script'
types:
  alpha__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  native:
    seq:
    - id: kind
      type: u1
      enum: kind_tag
    - id: storage
      type: bytes_dyn_uint30
enums:
  alpha__script_tag:
    0: michelson
    1: native
  kind_tag:
    0: clst
seq:
- id: alpha__script_tag
  type: u1
  enum: alpha__script_tag
- id: michelson
  type: alpha__scripted__contracts
  if: (alpha__script_tag == alpha__script_tag::michelson)
- id: native
  type: native
  if: (alpha__script_tag == alpha__script_tag::native)
