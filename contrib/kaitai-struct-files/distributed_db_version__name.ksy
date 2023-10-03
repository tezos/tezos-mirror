meta:
  id: distributed_db_version__name
  endian: be
types:
  fixed_bytes:
    seq:
    - id: size
      type: u4
    - id: value
      size: size
seq:
- id: distributed_db_version__name
  type: fixed_bytes
