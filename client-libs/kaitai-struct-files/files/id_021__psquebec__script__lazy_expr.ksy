meta:
  id: id_021__psquebec__script__lazy_expr
  endian: be
doc: ! 'Encoding id: 021-PsQuebec.script.lazy_expr'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
seq:
- id: id_021__psquebec__script__lazy_expr
  type: bytes_dyn_uint30
