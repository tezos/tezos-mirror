meta:
  id: user_activated__upgrades
  endian: be
doc: ! >-
  Encoding id: user_activated.upgrades

  Description: User activated upgrades: at given level, switch to given protocol.
types:
  user_activated__upgrades:
    seq:
    - id: user_activated__upgrades_entries
      type: user_activated__upgrades_entries
      repeat: eos
  user_activated__upgrades_entries:
    seq:
    - id: level
      type: s4
    - id: replacement_protocol
      size: 32
seq:
- id: len_user_activated__upgrades
  type: u4
  valid:
    max: 1073741823
- id: user_activated__upgrades
  type: user_activated__upgrades
  size: len_user_activated__upgrades
