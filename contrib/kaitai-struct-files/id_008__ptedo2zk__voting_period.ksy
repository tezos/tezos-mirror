meta:
  id: id_008__ptedo2zk__voting_period
  endian: be
doc: ! 'Encoding id: 008-PtEdo2Zk.voting_period'
enums:
  kind_tag:
    0: proposal
    1: testing_vote
    2: testing
    3: promotion_vote
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
