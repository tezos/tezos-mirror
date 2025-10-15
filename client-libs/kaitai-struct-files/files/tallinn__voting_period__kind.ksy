meta:
  id: tallinn__voting_period__kind
  endian: be
doc: ! 'Encoding id: tallinn.voting_period.kind'
enums:
  tallinn__voting_period__kind_tag:
    0: proposal
    1: exploration
    2: cooldown
    3: promotion
    4: adoption
seq:
- id: tallinn__voting_period__kind_tag
  type: u1
  enum: tallinn__voting_period__kind_tag
