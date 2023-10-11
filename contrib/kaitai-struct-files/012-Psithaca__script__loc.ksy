meta:
  id: id_012__psithaca__script__loc
  endian: be
types:
  micheline__location:
    meta:
      id: micheline__location
      endian: be
    doc: ! >-
      Canonical location in a Micheline expression: The location of a node in a Micheline
      expression tree in prefix order, with zero being the root and adding one for
      every basic node, sequence and primitive application.
    seq:
    - id: micheline__location
      type: s4
seq:
- id: micheline__location
  type: micheline__location
