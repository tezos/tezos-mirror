meta:
  id: id_007__psdelph1__fitness
  endian: be
doc: ! 'Encoding id: 007-PsDELPH1.fitness'
types:
  fitness:
    seq:
    - id: fitness_entries
      type: fitness_entries
      repeat: eos
  fitness_0:
    seq:
    - id: len_fitness
      type: s4
    - id: fitness
      type: fitness
      size: len_fitness
  fitness__elem:
    seq:
    - id: len_fitness__elem
      type: s4
    - id: fitness__elem
      size: len_fitness__elem
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
