meta:
  id: id_006__pscartha__block_header__contents
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.block_header.contents'
types:
  id_006__pscartha__block_header__alpha__unsigned_contents:
    seq:
    - id: priority
      type: u2
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
enums:
  bool:
    0: false
    255: true
seq:
- id: id_006__pscartha__block_header__alpha__unsigned_contents
  type: id_006__pscartha__block_header__alpha__unsigned_contents
