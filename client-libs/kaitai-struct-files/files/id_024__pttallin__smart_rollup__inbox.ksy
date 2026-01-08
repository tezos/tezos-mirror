meta:
  id: id_024__pttallin__smart_rollup__inbox
  endian: be
doc: ! 'Encoding id: 024-PtTALLiN.smart_rollup.inbox'
types:
  back_pointers:
    seq:
    - id: back_pointers_entries
      type: back_pointers_entries
      repeat: eos
  back_pointers_0:
    seq:
    - id: len_back_pointers
      type: u4be
      valid:
        max: 1073741823
    - id: back_pointers
      type: back_pointers
      size: len_back_pointers
  back_pointers_entries:
    seq:
    - id: smart_rollup_inbox_hash
      size: 32
  content:
    seq:
    - id: hash
      size: 32
    - id: level
      type: s4be
  n:
    seq:
    - id: n
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
  old_levels_messages:
    seq:
    - id: index
      type: n
    - id: content
      type: content
    - id: back_pointers
      type: back_pointers_0
seq:
- id: level
  type: s4be
- id: old_levels_messages
  type: old_levels_messages
