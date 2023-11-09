meta:
  id: p2p_peer__state
  endian: be
doc: ! >-
  Encoding id: p2p_peer.state

  Description: The state a peer connection can be in: accepted (when the connection
  is being established), running (when the connection is already established), disconnected
  (otherwise).
enums:
  p2p_peer__state:
    0: accepted
    1: running
    2: disconnected
seq:
- id: p2p_peer__state
  type: u1
  enum: p2p_peer__state
