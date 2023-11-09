meta:
  id: id_016__ptmumbai__constants__fixed
  endian: be
doc: ! 'Encoding id: 016-PtMumbai.constants.fixed'
types:
  n:
    seq:
    - id: n
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
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
- id: cache_layout_size
  type: u1
- id: michelson_maximum_type_size
  type: u2
- id: smart_rollup_max_wrapped_proof_binary_size
  type: s4
- id: smart_rollup_message_size_limit
  type: s4
- id: smart_rollup_max_number_of_messages_per_level
  type: n
