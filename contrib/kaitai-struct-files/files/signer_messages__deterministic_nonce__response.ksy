meta:
  id: signer_messages__deterministic_nonce__response
  endian: be
doc: ! 'Encoding id: signer_messages.deterministic_nonce.response'
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
- id: deterministic_nonce
  type: bytes_dyn_uint30
