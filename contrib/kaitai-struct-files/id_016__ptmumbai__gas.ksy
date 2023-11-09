meta:
  id: id_016__ptmumbai__gas
  endian: be
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
enums:
  id_016__ptmumbai__gas_tag:
    0: limited
    1: unaccounted
seq:
- id: id_016__ptmumbai__gas_tag
  type: u1
  enum: id_016__ptmumbai__gas_tag
- id: limited
  type: z
  if: (id_016__ptmumbai__gas_tag == id_016__ptmumbai__gas_tag::limited)
