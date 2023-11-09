meta:
  id: id_006__pscartha__script__loc
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.script.loc'
types:
  micheline__location:
    seq:
    - id: micheline__location
      type: s4
seq:
- id: micheline__location
  type: micheline__location
  doc: ! >-
    Canonical location in a Micheline expression: The location of a node in a Micheline
    expression tree in prefix order, with zero being the root and adding one for every
    basic node, sequence and primitive application.
