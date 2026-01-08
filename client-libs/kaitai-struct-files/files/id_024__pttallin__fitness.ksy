meta:
  id: id_024__pttallin__fitness
  endian: be
doc: ! 'Encoding id: 024-PtTALLiN.fitness'
types:
  locked_round:
    seq:
    - id: locked_round_tag
      type: u1
      enum: locked_round_tag
    - id: some
      type: s4be
      if: (locked_round_tag == locked_round_tag::some)
enums:
  locked_round_tag:
    0: none
    1: some
seq:
- id: level
  type: s4be
- id: locked_round
  type: locked_round
- id: predecessor_round
  type: s4be
- id: round
  type: s4be
