meta:
  id: ground__z
  endian: be
doc: Arbitrary precision integers
types:
  z:
    meta:
      id: z
      endian: be
    types:
      n_group:
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
    - id: n
      type: n_group
      repeat: until
      repeat-until: not (_.has_next)
  n:
    meta:
      id: n
      endian: be
    types:
      n_group:
        instances:
          has_next:
            value: ((b & 128) != 0)
          value:
            value: (b & 127)
        seq:
        - id: b
          type: u1
    seq:
    - id: n
      type: n_group
      repeat: until
      repeat-until: not (_.has_next)
seq:
- id: ground__z
  type: z
