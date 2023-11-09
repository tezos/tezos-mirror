meta:
  id: p2p_connection__pool_event
  endian: be
doc: ! >-
  An event that may happen during maintenance of and other operations on the p2p connection
  pool. Typically, it includes connection errors, peer swaps, etc.
types:
  identity:
    seq:
    - id: identity_field0
      type: p2p_connection__id
      doc: p2p_connection__id
    - id: identity_field1
      size: 16
      doc: crypto_box__public_key_hash
  p2p_address:
    doc: An address for locating peers.
    seq:
    - id: len_p2p_address
      type: s4
    - id: p2p_address
      size: len_p2p_address
  p2p_connection__id:
    doc: The identifier for a p2p connection. It includes an address and a port number.
    seq:
    - id: addr
      type: p2p_address
    - id: port_tag
      type: u1
      enum: bool
    - id: port
      type: u2
      if: (port_tag == bool::true)
  p2p_connection__pool_event_accepting_request:
    seq:
    - id: point
      type: p2p_point__id
    - id: id_point
      type: p2p_connection__id
    - id: peer_id
      size: 16
  p2p_connection__pool_event_connection_established:
    seq:
    - id: id_point
      type: p2p_connection__id
    - id: peer_id
      size: 16
  p2p_connection__pool_event_rejecting_request:
    seq:
    - id: point
      type: p2p_point__id
    - id: id_point
      type: p2p_connection__id
    - id: peer_id
      size: 16
  p2p_connection__pool_event_request_rejected:
    seq:
    - id: point
      type: p2p_point__id
    - id: identity_tag
      type: u1
      enum: bool
    - id: identity
      type: identity
      if: (identity_tag == bool::true)
  p2p_point__id:
    doc: Identifier for a peer point
    seq:
    - id: len_p2p_point__id
      type: s4
    - id: p2p_point__id
      size: len_p2p_point__id
enums:
  bool:
    0: false
    255: true
  p2p_connection__pool_event_tag:
    0: too_few_connections
    1: too_many_connections
    2: new_point
    3: new_peer
    4: incoming_connection
    5: outgoing_connection
    6: authentication_failed
    7: accepting_request
    8: rejecting_request
    9: request_rejected
    10: connection_established
    11: disconnection
    12: external_disconnection
    13: gc_points
    14: gc_peer_ids
    15: swap_request_received
    16: swap_ack_received
    17: swap_request_sent
    18: swap_ack_sent
    19: swap_request_ignored
    20: swap_success
    21: swap_failure
    22: bootstrap_sent
    23: bootstrap_received
    24: advertise_sent
    25: advertise_received
seq:
- id: p2p_connection__pool_event_tag
  type: u1
  enum: p2p_connection__pool_event_tag
- id: p2p_connection__pool_event_new_point
  type: p2p_point__id
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::new_point)
- id: p2p_connection__pool_event_new_peer
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::new_peer)
- id: p2p_connection__pool_event_incoming_connection
  type: p2p_point__id
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::incoming_connection)
- id: p2p_connection__pool_event_outgoing_connection
  type: p2p_point__id
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::outgoing_connection)
- id: p2p_connection__pool_event_authentication_failed
  type: p2p_point__id
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::authentication_failed)
- id: p2p_connection__pool_event_accepting_request
  type: p2p_connection__pool_event_accepting_request
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::accepting_request)
- id: p2p_connection__pool_event_rejecting_request
  type: p2p_connection__pool_event_rejecting_request
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::rejecting_request)
- id: p2p_connection__pool_event_request_rejected
  type: p2p_connection__pool_event_request_rejected
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::request_rejected)
- id: p2p_connection__pool_event_connection_established
  type: p2p_connection__pool_event_connection_established
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::connection_established)
- id: p2p_connection__pool_event_disconnection
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::disconnection)
- id: p2p_connection__pool_event_external_disconnection
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::external_disconnection)
- id: p2p_connection__pool_event_swap_request_received
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::swap_request_received)
- id: p2p_connection__pool_event_swap_ack_received
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::swap_ack_received)
- id: p2p_connection__pool_event_swap_request_sent
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::swap_request_sent)
- id: p2p_connection__pool_event_swap_ack_sent
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::swap_ack_sent)
- id: p2p_connection__pool_event_swap_request_ignored
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::swap_request_ignored)
- id: p2p_connection__pool_event_swap_success
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::swap_success)
- id: p2p_connection__pool_event_swap_failure
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::swap_failure)
- id: p2p_connection__pool_event_bootstrap_sent
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::bootstrap_sent)
- id: p2p_connection__pool_event_bootstrap_received
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::bootstrap_received)
- id: p2p_connection__pool_event_advertise_sent
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::advertise_sent)
- id: p2p_connection__pool_event_advertise_received
  size: 16
  if: (p2p_connection__pool_event_tag == p2p_connection__pool_event_tag::advertise_received)
