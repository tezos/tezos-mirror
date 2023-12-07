meta:
  id: p2p_identity
  endian: be
doc: ! >-
  Encoding id: p2p_identity

  Description: The identity of a peer. This includes cryptographic keys as well as
  a proof-of-work.
enums:
  bool:
    0: false
    255: true
seq:
- id: peer_id_tag
  type: u1
  enum: bool
- id: peer_id
  size: 16
  if: (peer_id_tag == bool::true)
- id: public_key
  size: 32
- id: secret_key
  size: 32
- id: proof_of_work_stamp
  size: 24
