meta:
  id: id_008__ptedo2zk__gas__cost
  endian: be
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
- id: id_008__ptedo2zk__gas__cost
  type: z
