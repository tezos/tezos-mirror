meta:
  id: id_017__ptnairob__level
  endian: be
doc: ! 'Encoding id: 017-PtNairob.level'
enums:
  bool:
    0: false
    255: true
seq:
- id: level
  type: s4
  doc: ! >-
    The level of the block relative to genesis. This is also the Shell's notion of
    level.
- id: level_position
  type: s4
  doc: ! >-
    The level of the block relative to the successor of the genesis block. More precisely,
    it is the position of the block relative to the block that starts the "Alpha family"
    of protocols, which includes all protocols except Genesis (that is, from 001 onwards).
- id: cycle
  type: s4
  doc: ! >-
    The current cycle's number. Note that cycles are a protocol-specific notion. As
    a result, the cycle number starts at 0 with the first block of the Alpha family
    of protocols.
- id: cycle_position
  type: s4
  doc: The current level of the block relative to the first block of the current cycle.
- id: expected_commitment
  type: u1
  enum: bool
  doc: Tells whether the baker of this block has to commit a seed nonce hash.
