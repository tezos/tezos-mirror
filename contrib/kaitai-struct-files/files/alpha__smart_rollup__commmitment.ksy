meta:
  id: alpha__smart_rollup__commmitment
  endian: be
doc: ! 'Encoding id: alpha.smart_rollup.commmitment'
seq:
- id: compressed_state
  size: 32
- id: inbox_level
  type: s4
- id: predecessor
  size: 32
- id: number_of_ticks
  type: s8
