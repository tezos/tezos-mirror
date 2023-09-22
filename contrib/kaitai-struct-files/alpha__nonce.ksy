meta:
  id: alpha__nonce
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: alpha__nonce
  type: fixed_bytes
