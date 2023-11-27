meta:
  id: id_012__psithaca__constants__fixed
  endian: be
doc: ! 'Encoding id: 012-Psithaca.constants.fixed'
types:
  cache_layout:
    seq:
    - id: cache_layout_entries
      type: cache_layout_entries
      repeat: eos
  cache_layout_0:
    seq:
    - id: len_cache_layout
      type: s4
    - id: cache_layout
      type: cache_layout
      size: len_cache_layout
  cache_layout_entries:
    seq:
    - id: cache_layout_elt
      type: s8
seq:
- id: proof_of_work_nonce_size
  type: u1
- id: nonce_length
  type: u1
- id: max_anon_ops_per_block
  type: u1
- id: max_operation_data_length
  type: s4
- id: max_proposals_per_delegate
  type: u1
- id: max_micheline_node_count
  type: s4
- id: max_micheline_bytes_limit
  type: s4
- id: max_allowed_global_constants_depth
  type: s4
- id: cache_layout
  type: cache_layout_0
- id: michelson_maximum_type_size
  type: u2
