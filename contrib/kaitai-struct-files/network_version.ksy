meta:
  id: network_version
  endian: be
doc: ! >-
  Encoding id: network_version

  Description: A version number for the network protocol (includes distributed DB
  version and p2p version)
types:
  distributed_db_version__name:
    doc: A name for the distributed DB protocol
    seq:
    - id: len_distributed_db_version__name
      type: s4
    - id: distributed_db_version__name
      size: len_distributed_db_version__name
seq:
- id: chain_name
  type: distributed_db_version__name
- id: distributed_db_version
  type: u2
  doc: A version number for the distributed DB protocol
- id: p2p_version
  type: u2
  doc: A version number for the p2p layer.
