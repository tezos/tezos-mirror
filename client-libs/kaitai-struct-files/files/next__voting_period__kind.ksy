meta:
  id: next__voting_period__kind
  endian: be
doc: ! 'Encoding id: next.voting_period.kind'
enums:
  next__voting_period__kind_tag:
    0: proposal
    1: exploration
    2: cooldown
    3: promotion
    4: adoption
seq:
- id: next__voting_period__kind_tag
  type: u1
  enum: next__voting_period__kind_tag
