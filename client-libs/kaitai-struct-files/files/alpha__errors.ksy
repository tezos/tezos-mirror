meta:
  id: alpha__errors
  endian: be
doc: ! >-
  Encoding id: alpha.errors

  Description: The full list of RPC errors would be too long to include.It is

  available through the RPC `/errors` (GET).
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
seq:
- id: alpha__errors
  type: bytes_dyn_uint30
