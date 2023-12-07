meta:
  id: alpha__voting_period__kind
  endian: be
doc: ! 'Encoding id: alpha.voting_period.kind'
enums:
  alpha__voting_period__kind_tag:
    0: proposal
    1: exploration
    2: cooldown
    3: promotion
    4: adoption
seq:
- id: alpha__voting_period__kind_tag
  type: u1
  enum: alpha__voting_period__kind_tag
