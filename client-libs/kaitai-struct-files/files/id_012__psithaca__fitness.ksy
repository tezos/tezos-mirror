meta:
  id: id_012__psithaca__fitness
  endian: be
doc: ! 'Encoding id: 012-Psithaca.fitness'
types:
  locked_round:
    seq:
    - id: locked_round_tag
      type: u1
      enum: locked_round_tag
    - id: some
      type: s4
      if: (locked_round_tag == locked_round_tag::some)
enums:
  locked_round_tag:
    0: none
    1: some
seq:
- id: level
  type: s4
- id: locked_round
  type: locked_round
- id: predecessor_round
  type: s4
- id: round
  type: s4
