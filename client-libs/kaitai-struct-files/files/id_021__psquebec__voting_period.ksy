meta:
  id: id_021__psquebec__voting_period
  endian: be
doc: ! 'Encoding id: 021-PsQuebec.voting_period'
enums:
  kind_tag:
    0: proposal
    1: exploration
    2: cooldown
    3: promotion
    4: adoption
seq:
- id: index
  type: s4be
  doc: ! >-
    The voting period's index. Starts at 0 with the first block of the Alpha family
    of protocols.
- id: kind
  type: u1
  enum: kind_tag
  doc: One of the several kinds of periods in the voting procedure.
- id: start_position
  type: s4be
  doc: ! >-
    The relative position of the first level of the period with respect to the first
    level of the Alpha family of protocols.
