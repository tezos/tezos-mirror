meta:
  id: alpha__errors
  endian: be
doc: ! >-
  Encoding id: alpha.errors

  Description: The full list of RPC errors would be too long to include.It is

  available through the RPC `/errors` (GET).
seq:
- id: len_alpha__errors
  type: s4
- id: alpha__errors
  size: len_alpha__errors
