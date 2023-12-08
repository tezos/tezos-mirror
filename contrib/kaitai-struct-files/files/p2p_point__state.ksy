meta:
  id: p2p_point__state
  endian: be
doc: ! >-
  Encoding id: p2p_point.state

  Description: The state a connection to a peer point can be in: requested (connection
  open from here), accepted (handshake), running (connection already established),
  disconnected (no connection).
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
- id: accepted
  size: 16
  if: (p2p_point__state_tag == p2p_point__state_tag::accepted)
- id: running
  size: 16
  if: (p2p_point__state_tag == p2p_point__state_tag::running)
