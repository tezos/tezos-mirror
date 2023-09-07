meta:
  id: ground__N
  endian: be
types:
  group:
    instances:
      has_next:
        value: ((b & 128) != 0)
      value:
        value: (b & 127)
    seq:
    - id: b
      type: u1
seq:
- id: groups
  type: group
  repeat: until
  repeat-until: not (_.has_next)
