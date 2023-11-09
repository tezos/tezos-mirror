meta:
  id: id_006__pscartha__gas__cost
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.gas.cost'
types:
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
  z:
    seq:
    - id: has_tail
      type: b1be
    - id: sign
      type: b1be
    - id: payload
      type: b6be
    - id: tail
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
      if: has_tail.as<bool>
seq:
- id: allocations
  type: z
- id: steps
  type: z
- id: reads
  type: z
- id: writes
  type: z
- id: bytes_read
  type: z
- id: bytes_written
  type: z
