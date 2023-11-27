meta:
  id: mempool
  endian: be
doc: ! >-
  Encoding id: mempool

  Description: A batch of operation. This format is used to gossip operations between
  peers.
types:
  known_valid:
    seq:
    - id: known_valid_entries
      type: known_valid_entries
      repeat: eos
  known_valid_0:
    seq:
    - id: len_known_valid
      type: s4
    - id: known_valid
      type: known_valid
      size: len_known_valid
  known_valid_entries:
    seq:
    - id: operation_hash
      size: 32
  pending:
    seq:
    - id: pending_entries
      type: pending_entries
      repeat: eos
  pending_0:
    seq:
    - id: len_pending
      type: s4
    - id: pending
      type: pending
      size: len_pending
  pending_1:
    seq:
    - id: len_pending
      type: s4
    - id: pending
      type: pending_0
      size: len_pending
  pending_entries:
    seq:
    - id: operation_hash
      size: 32
seq:
- id: known_valid
  type: known_valid_0
- id: pending
  type: pending_1
