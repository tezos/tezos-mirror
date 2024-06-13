meta:
  id: id_020__psparisc__smart_rollup__commmitment
  endian: be
doc: ! 'Encoding id: 020-PsParisC.smart_rollup.commmitment'
seq:
- id: compressed_state
  size: 32
- id: inbox_level
  type: s4be
- id: predecessor
  size: 32
- id: number_of_ticks
  type: s8be
