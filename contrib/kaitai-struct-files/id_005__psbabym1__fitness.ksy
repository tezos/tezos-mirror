meta:
  id: id_005__psbabym1__fitness
  endian: be
doc: ! 'Encoding id: 005-PsBabyM1.fitness'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  fitness:
    seq:
    - id: fitness_entries
      type: fitness_entries
      repeat: eos
  fitness_0:
    seq:
    - id: len_fitness
      type: u4
      valid:
        max: 1073741823
    - id: fitness
      type: fitness
      size: len_fitness
  fitness__elem:
    seq:
    - id: fitness__elem
      type: bytes_dyn_uint30
  fitness_entries:
    seq:
    - id: fitness__elem
      type: fitness__elem
seq:
- id: fitness
  type: fitness_0
  doc: ! >-
    Block fitness: The fitness, or score, of a block, that allow the Tezos to decide
    which chain is the best. A fitness value is a list of byte sequences. They are
    compared as follows: shortest lists are smaller; lists of the same length are
    compared according to the lexicographical order.
