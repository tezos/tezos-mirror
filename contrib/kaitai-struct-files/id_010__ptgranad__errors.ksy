meta:
  id: id_010__ptgranad__errors
  endian: be
doc: ! >-
  Encoding id: 010-PtGRANAD.errors

  Description: The full list of RPC errors would be too long to include.It is

  available through the RPC `/errors` (GET).
seq:
- id: len_id_010__ptgranad__errors
  type: s4
- id: id_010__ptgranad__errors
  size: len_id_010__ptgranad__errors
