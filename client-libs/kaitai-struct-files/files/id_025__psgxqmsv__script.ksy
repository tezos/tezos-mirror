meta:
  id: id_025__psgxqmsv__script
  endian: be
doc: ! 'Encoding id: 025-PsGXQmSv.script'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  id_025__psgxqmsv__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  native:
    seq:
    - id: kind
      type: u1
      enum: kind_tag
    - id: storage
      type: bytes_dyn_uint30
enums:
  id_025__psgxqmsv__script_tag:
    0: michelson
    1: native
  kind_tag:
    0: clst
seq:
- id: id_025__psgxqmsv__script_tag
  type: u1
  enum: id_025__psgxqmsv__script_tag
- id: michelson
  type: id_025__psgxqmsv__scripted__contracts
  if: (id_025__psgxqmsv__script_tag == id_025__psgxqmsv__script_tag::michelson)
- id: native
  type: native
  if: (id_025__psgxqmsv__script_tag == id_025__psgxqmsv__script_tag::native)
