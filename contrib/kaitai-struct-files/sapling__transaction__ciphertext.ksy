meta:
  id: sapling__transaction__ciphertext
  endian: be
  imports:
  - sapling__transaction__commitment_value
doc: ! 'Encoding id: sapling.transaction.ciphertext'
types:
  payload_enc:
    seq:
    - id: len_payload_enc
      type: s4
    - id: payload_enc
      size: len_payload_enc
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
  type: payload_enc
- id: nonce_enc
  size: 24
- id: payload_out
  size: 80
- id: nonce_out
  size: 24
