meta:
  id: id_007__psdelph1__gas__cost
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
instances:
  is_negative:
    value: (((groups[0].value) >> 6) == 1)
seq:
- id: groups
  type: group
  repeat: until
  repeat-until: not (_.has_next)
