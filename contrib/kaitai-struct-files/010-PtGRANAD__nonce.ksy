meta:
  id: id_010__ptgranad__nonce
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: id_010__ptgranad__nonce
  type: fixed_bytes
