meta:
  id: id_009__psfloren__voting_period
  endian: be
doc: ! 'Encoding id: 009-PsFLoren.voting_period'
enums:
  kind_tag:
    0: proposal
    1: exploration
    2: cooldown
    3: promotion
    4: adoption
seq:
- id: index
  type: s4
  doc: The voting period's index. Starts at 0 with the first block of protocol alpha.
- id: kind
  type: u1
  enum: kind_tag
- id: start_position
  type: s4
