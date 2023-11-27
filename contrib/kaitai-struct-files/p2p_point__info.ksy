meta:
  id: p2p_point__info
  endian: be
doc: ! >-
  Information about a peer point. Includes flags, state, and records about past events.
types:
  last_disconnection:
    seq:
    - id: last_disconnection_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_disconnection_field1
      type: s8
      doc: ! >-
        A timestamp as seen by the underlying, local computer: subsecond-level precision,
        epoch or rfc3339 based.


        timestamp__system
  last_established_connection:
    seq:
    - id: last_established_connection_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_established_connection_field1
      type: s8
      doc: ! >-
        A timestamp as seen by the underlying, local computer: subsecond-level precision,
        epoch or rfc3339 based.


        timestamp__system
  last_rejected_connection:
    seq:
    - id: last_rejected_connection_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_rejected_connection_field1
      type: s8
      doc: ! >-
        A timestamp as seen by the underlying, local computer: subsecond-level precision,
        epoch or rfc3339 based.


        timestamp__system
  last_seen:
    seq:
    - id: last_seen_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_seen_field1
      type: s8
      doc: ! >-
        A timestamp as seen by the underlying, local computer: subsecond-level precision,
        epoch or rfc3339 based.


        timestamp__system
  p2p_point__state:
    doc: ! >-
      The state a connection to a peer point can be in: requested (connection open
      from here), accepted (handshake), running (connection already established),
      disconnected (no connection).
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
enums:
  bool:
    0: false
    255: true
  p2p_point__state_tag:
    0: requested
    1: accepted
    2: running
    3: disconnected
seq:
- id: trusted
  type: u1
  enum: bool
- id: greylisted_until_tag
  type: u1
  enum: bool
- id: greylisted_until
  type: s8
  if: (greylisted_until_tag == bool::true)
  doc: ! >-
    A timestamp as seen by the underlying, local computer: subsecond-level precision,
    epoch or rfc3339 based.
- id: state
  type: p2p_point__state
- id: p2p_peer_id_tag
  type: u1
  enum: bool
- id: p2p_peer_id
  size: 16
  if: (p2p_peer_id_tag == bool::true)
- id: last_failed_connection_tag
  type: u1
  enum: bool
- id: last_failed_connection
  type: s8
  if: (last_failed_connection_tag == bool::true)
  doc: ! >-
    A timestamp as seen by the underlying, local computer: subsecond-level precision,
    epoch or rfc3339 based.
- id: last_rejected_connection_tag
  type: u1
  enum: bool
- id: last_rejected_connection
  type: last_rejected_connection
  if: (last_rejected_connection_tag == bool::true)
- id: last_established_connection_tag
  type: u1
  enum: bool
- id: last_established_connection
  type: last_established_connection
  if: (last_established_connection_tag == bool::true)
- id: last_disconnection_tag
  type: u1
  enum: bool
- id: last_disconnection
  type: last_disconnection
  if: (last_disconnection_tag == bool::true)
- id: last_seen_tag
  type: u1
  enum: bool
- id: last_seen
  type: last_seen
  if: (last_seen_tag == bool::true)
- id: last_miss_tag
  type: u1
  enum: bool
- id: last_miss
  type: s8
  if: (last_miss_tag == bool::true)
  doc: ! >-
    A timestamp as seen by the underlying, local computer: subsecond-level precision,
    epoch or rfc3339 based.
- id: expected_peer_id_tag
  type: u1
  enum: bool
- id: expected_peer_id
  size: 16
  if: (expected_peer_id_tag == bool::true)
