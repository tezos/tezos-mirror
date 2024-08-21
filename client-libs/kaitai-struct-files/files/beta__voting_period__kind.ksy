meta:
  id: beta__voting_period__kind
  endian: be
doc: ! 'Encoding id: beta.voting_period.kind'
enums:
  beta__voting_period__kind_tag:
    0: proposal
    1: exploration
    2: cooldown
    3: promotion
    4: adoption
seq:
- id: beta__voting_period__kind_tag
  type: u1
  enum: beta__voting_period__kind_tag
