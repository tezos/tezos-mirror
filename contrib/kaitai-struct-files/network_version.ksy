meta:
  id: network_version
  endian: be
  imports:
  - distributed_db_version
  - distributed_db_version__name
  - p2p_version
doc: ! >-
  Encoding id: network_version

  Description: A version number for the network protocol (includes distributed DB
  version and p2p version)
seq:
- id: chain_name
  type: distributed_db_version__name
- id: distributed_db_version
  type: distributed_db_version
- id: p2p_version
  type: p2p_version
