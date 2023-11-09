meta:
  id: p2p_stat
  endian: be
doc: ! 'Encoding id: p2p_stat

  Description: Statistics about the p2p network.'
seq:
- id: total_sent
  type: s8
- id: total_recv
  type: s8
- id: current_inflow
  type: s4
- id: current_outflow
  type: s4
