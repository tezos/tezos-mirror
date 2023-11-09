meta:
  id: block_header__shell
  endian: be
  imports:
  - timestamp__protocol
doc: ! >-
  Encoding id: block_header.shell

  Description: Block header's shell-related content. It contains information such
  as the block level, its predecessor and timestamp.
types:
  fitness:
    doc: ! >-
      Block fitness: The fitness, or score, of a block, that allow the Tezos to decide
      which chain is the best. A fitness value is a list of byte sequences. They are
      compared as follows: shortest lists are smaller; lists of the same length are
      compared according to the lexicographical order.
    seq:
    - id: len_fitness
      type: s4
    - id: fitness
      type: fitness_entries
      size: len_fitness
      repeat: eos
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
- id: level
  type: s4
- id: proto
  type: u1
- id: predecessor
  size: 32
- id: timestamp
  type: timestamp__protocol
- id: validation_pass
  type: u1
- id: operations_hash
  size: 32
- id: fitness
  type: fitness
- id: context
  size: 32
