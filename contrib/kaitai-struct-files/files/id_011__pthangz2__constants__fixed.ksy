meta:
  id: id_011__pthangz2__constants__fixed
  endian: be
doc: ! 'Encoding id: 011-PtHangz2.constants.fixed'
types:
  cache_layout:
    seq:
    - id: cache_layout_entries
      type: cache_layout_entries
      repeat: eos
  cache_layout_0:
    seq:
    - id: len_cache_layout
      type: u4
      valid:
        max: 1073741823
    - id: cache_layout
      type: cache_layout
      size: len_cache_layout
  cache_layout_entries:
    seq:
    - id: cache_layout_elt
      type: s8
  int31:
    seq:
    - id: int31
      type: s4
      valid:
        min: -1073741824
        max: 1073741823
seq:
- id: proof_of_work_nonce_size
  type: u1
- id: nonce_length
  type: u1
- id: max_anon_ops_per_block
  type: u1
- id: max_operation_data_length
  type: int31
- id: max_proposals_per_delegate
  type: u1
- id: max_micheline_node_count
  type: int31
- id: max_micheline_bytes_limit
  type: int31
- id: max_allowed_global_constants_depth
  type: int31
- id: cache_layout
  type: cache_layout_0
- id: michelson_maximum_type_size
  type: u2
