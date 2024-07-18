meta:
  id: beta__block_header__contents
  endian: be
doc: ! 'Encoding id: beta.block_header.contents'
types:
  beta__block_header__alpha__unsigned_contents:
    seq:
    - id: payload_hash
      size: 32
    - id: payload_round
      type: s4be
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
    - id: per_block_votes
      type: beta__per_block_votes
  beta__per_block_votes:
    seq:
    - id: beta__per_block_votes_tag
      type: u1
      enum: beta__per_block_votes_tag
enums:
  beta__per_block_votes_tag:
    0: case_0
    1: case_1
    2: case_2
    4: case_4
    5: case_5
    6: case_6
    8: case_8
    9: case_9
    10: case_10
  bool:
    0: false
    255: true
seq:
- id: beta__block_header__alpha__unsigned_contents
  type: beta__block_header__alpha__unsigned_contents
