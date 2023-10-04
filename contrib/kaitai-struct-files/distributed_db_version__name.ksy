meta:
  id: distributed_db_version__name
  endian: be
doc: A name for the distributed DB protocol
types:
  distributed_db_version__name:
    meta:
      id: distributed_db_version__name
      endian: be
    seq:
    - id: distributed_db_version__name
      type: variable size bytes
seq:
- id: len_distributed_db_version__name
  type: s4
- id: distributed_db_version__name
  type: distributed_db_version__name
  size: len_distributed_db_version__name
