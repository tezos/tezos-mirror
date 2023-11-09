meta:
  id: p2p_point__pool_event
  endian: be
doc: ! >-
  Events happening during maintenance of and operations on a peer point pool (such
  as connections, disconnections, connection requests).
types:
  p2p_point__pool_event_field1:
    seq:
    - id: p2p_point__pool_event_field1_tag
      type: u1
      enum: p2p_point__pool_event_field1_tag
    - id: accepting_request
      size: 16
      if: (p2p_point__pool_event_field1_tag == p2p_point__pool_event_field1_tag::accepting_request)
    - id: rejecting_request
      size: 16
      if: (p2p_point__pool_event_field1_tag == p2p_point__pool_event_field1_tag::rejecting_request)
    - id: rejecting_rejected
      type: rejecting_rejected
      if: (p2p_point__pool_event_field1_tag == p2p_point__pool_event_field1_tag::rejecting_rejected)
    - id: connection_established
      size: 16
      if: (p2p_point__pool_event_field1_tag == p2p_point__pool_event_field1_tag::connection_established)
    - id: disconnection
      size: 16
      if: (p2p_point__pool_event_field1_tag == p2p_point__pool_event_field1_tag::disconnection)
    - id: external_disconnection
      size: 16
      if: (p2p_point__pool_event_field1_tag == p2p_point__pool_event_field1_tag::external_disconnection)
  rejecting_rejected:
    seq:
    - id: p2p_peer_id_tag
      type: u1
      enum: bool
    - id: p2p_peer_id
      size: 16
      if: (p2p_peer_id_tag == bool::true)
enums:
  bool:
    0: false
    255: true
  p2p_point__pool_event_field1_tag:
    0: outgoing_request
    1: accepting_request
    2: rejecting_request
    3: rejecting_rejected
    4: connection_established
    5: disconnection
    6: external_disconnection
seq:
- id: p2p_point__pool_event_field0
  type: s8
  doc: ! >-
    A timestamp as seen by the underlying, local computer: subsecond-level precision,
    epoch or rfc3339 based.


    timestamp__system
- id: p2p_point__pool_event_field1
  type: p2p_point__pool_event_field1
