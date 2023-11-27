meta:
  id: block_locator
  endian: be
doc: ! "A sparse block locator \xE0 la Bitcoin"
types:
  block_header:
    doc: ! 'Block header: Block header. It contains both shell and protocol specific
      data.'
    seq:
    - id: block_header__shell
      type: block_header__shell
    - id: protocol_data
      size-eos: true
  block_header__shell:
    doc: ! >-
      Shell header: Block header's shell-related content. It contains information
      such as the block level, its predecessor and timestamp.
    seq:
    - id: level
      type: s4
    - id: proto
      type: u1
    - id: predecessor
      size: 32
    - id: timestamp
      type: s8
      doc: ! 'A timestamp as seen by the protocol: second-level precision, epoch based.'
    - id: validation_pass
      type: u1
    - id: operations_hash
      size: 32
    - id: fitness
      type: fitness
    - id: context
      size: 32
  current_head:
    seq:
    - id: len_current_head
      type: s4
    - id: current_head
      type: block_header
      size: len_current_head
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
  history_entries:
    seq:
    - id: block_hash
      size: 32
seq:
- id: current_head
  type: current_head
- id: history
  type: history_entries
  repeat: eos
