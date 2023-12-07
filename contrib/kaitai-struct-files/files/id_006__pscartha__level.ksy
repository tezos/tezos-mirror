meta:
  id: id_006__pscartha__level
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.level'
enums:
  bool:
    0: false
    255: true
seq:
- id: level
  type: s4
  doc: ! >-
    The level of the block relative to genesis. This is also the Shell's notion of
    level
- id: level_position
  type: s4
  doc: ! >-
    The level of the block relative to the block that starts protocol alpha. This
    is specific to the protocol alpha. Other protocols might or might not include
    a similar notion.
- id: cycle
  type: s4
  doc: ! >-
    The current cycle's number. Note that cycles are a protocol-specific notion. As
    a result, the cycle number starts at 0 with the first block of protocol alpha.
- id: cycle_position
  type: s4
  doc: The current level of the block relative to the first block of the current cycle.
- id: voting_period
  type: s4
  doc: ! >-
    The current voting period's index. Note that cycles are a protocol-specific notion.
    As a result, the voting period index starts at 0 with the first block of protocol
    alpha.
- id: voting_period_position
  type: s4
  doc: ! >-
    The current level of the block relative to the first block of the current voting
    period.
- id: expected_commitment
  type: u1
  enum: bool
  doc: Tells wether the baker of this block has to commit a seed nonce hash.
