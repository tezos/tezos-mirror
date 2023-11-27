meta:
  id: p2p_point__state
  endian: be
doc: ! >-
  The state a connection to a peer point can be in: requested (connection open from
  here), accepted (handshake), running (connection already established), disconnected
  (no connection).
enums:
  p2p_point__state_tag:
    0: requested
    1: accepted
    2: running
    3: disconnected
seq:
- id: p2p_point__state_tag
  type: u1
  enum: p2p_point__state_tag
- id: p2p_point__state_accepted
  size: 16
  if: (p2p_point__state_tag == p2p_point__state_tag::accepted)
- id: p2p_point__state_running
  size: 16
  if: (p2p_point__state_tag == p2p_point__state_tag::running)
