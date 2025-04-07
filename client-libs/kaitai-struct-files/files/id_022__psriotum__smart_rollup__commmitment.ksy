meta:
  id: id_022__psriotum__smart_rollup__commmitment
  endian: be
doc: ! 'Encoding id: 022-PsRiotum.smart_rollup.commmitment'
seq:
- id: compressed_state
  size: 32
- id: inbox_level
  type: s4be
- id: predecessor
  size: 32
- id: number_of_ticks
  type: s8be
