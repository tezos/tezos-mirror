meta:
  id: next__block_header
  endian: be
  imports:
  - block_header__shell
doc: ! 'Encoding id: next.block_header'
types:
  next__block_header__alpha__full_header:
    seq:
    - id: next__block_header__alpha__full_header
      type: block_header__shell
    - id: next__block_header__alpha__signed_contents
      type: next__block_header__alpha__signed_contents
  next__block_header__alpha__signed_contents:
    seq:
    - id: next__block_header__alpha__unsigned_contents
      type: next__block_header__alpha__unsigned_contents
    - id: signature
      size-eos: true
  next__block_header__alpha__unsigned_contents:
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
      type: next__per_block_votes
  next__per_block_votes:
    seq:
    - id: next__per_block_votes_tag
      type: u1
      enum: next__per_block_votes_tag
enums:
  bool:
    0: false
    255: true
  next__per_block_votes_tag:
    0: case_0
    1: case_1
    2: case_2
    4: case_4
    5: case_5
    6: case_6
    8: case_8
    9: case_9
    10: case_10
seq:
- id: next__block_header__alpha__full_header
  type: next__block_header__alpha__full_header
