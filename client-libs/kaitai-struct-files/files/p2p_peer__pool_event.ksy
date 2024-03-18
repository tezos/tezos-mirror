meta:
  id: p2p_peer__pool_event
  endian: be
  imports:
  - p2p_address
  - timestamp__system
doc: ! >-
  Encoding id: p2p_peer.pool_event

  Description: An event that may happen during maintenance of and other operations
  on the connection to a specific peer.
enums:
  bool:
    0: false
    255: true
  kind:
    0: incoming_request
    1: rejecting_request
    2: request_rejected
    3: connection_established
    4: disconnection
    5: external_disconnection
seq:
- id: kind
  type: u1
  enum: kind
- id: timestamp
  type: timestamp__system
- id: addr
  type: p2p_address
- id: port_tag
  type: u1
  enum: bool
- id: port
  type: u2
  if: (port_tag == bool::true)
