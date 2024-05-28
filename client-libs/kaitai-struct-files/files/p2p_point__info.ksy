meta:
  id: p2p_point__info
  endian: be
  imports:
  - p2p_point__state
  - timestamp__system
doc: ! >-
  Encoding id: p2p_point.info

  Description: Information about a peer point. Includes flags, state, and records
  about past events.
types:
  last_disconnection:
    seq:
    - id: last_disconnection_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_disconnection_field1
      type: timestamp__system
  last_established_connection:
    seq:
    - id: last_established_connection_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_established_connection_field1
      type: timestamp__system
  last_rejected_connection:
    seq:
    - id: last_rejected_connection_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_rejected_connection_field1
      type: timestamp__system
  last_seen:
    seq:
    - id: last_seen_field0
      size: 16
      doc: crypto_box__public_key_hash
    - id: last_seen_field1
      type: timestamp__system
enums:
  bool:
    0: false
    255: true
seq:
- id: trusted
  type: u1
  enum: bool
- id: greylisted_until_tag
  type: u1
  enum: bool
- id: greylisted_until
  type: timestamp__system
  if: (greylisted_until_tag == bool::true)
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
  type: timestamp__system
  if: (last_failed_connection_tag == bool::true)
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
  type: timestamp__system
  if: (last_miss_tag == bool::true)
- id: expected_peer_id_tag
  type: u1
  enum: bool
- id: expected_peer_id
  size: 16
  if: (expected_peer_id_tag == bool::true)
