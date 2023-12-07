meta:
  id: sapling__transaction__ciphertext
  endian: be
  imports:
  - sapling__transaction__commitment_value
doc: ! 'Encoding id: sapling.transaction.ciphertext'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  sapling__dh__epk:
    seq:
    - id: sapling__dh__epk
      size: 32
seq:
- id: cv
  type: sapling__transaction__commitment_value
- id: epk
  type: sapling__dh__epk
- id: payload_enc
  type: bytes_dyn_uint30
- id: nonce_enc
  size: 24
- id: payload_out
  size: 80
- id: nonce_out
  size: 24
