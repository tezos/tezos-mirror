meta:
  id: id_012__psithaca__constants
  endian: be
types:
  cache_layout:
    seq:
    - id: len_cache_layout
      type: s4
    - id: cache_layout
      type: cache_layout_entries
      size: len_cache_layout
      repeat: eos
  cache_layout_entries:
    seq:
    - id: cache_layout_elt
      type: s8
  delegate_selection:
    seq:
    - id: delegate_selection_tag
      type: u1
      enum: delegate_selection_tag
    - id: round_robin_over_delegates
      type: round_robin_over_delegates
      if: (delegate_selection_tag == delegate_selection_tag::round_robin_over_delegates)
  minimal_participation_ratio:
    seq:
    - id: numerator
      type: u2
    - id: denominator
      type: u2
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
  public_key:
    doc: A Ed25519, Secp256k1, or P256 public key
    seq:
    - id: public_key_tag
      type: u1
      enum: public_key_tag
    - id: ed25519
      size: 32
      if: (public_key_tag == public_key_tag::ed25519)
    - id: secp256k1
      size: 33
      if: (public_key_tag == public_key_tag::secp256k1)
    - id: p256
      size: 33
      if: (public_key_tag == public_key_tag::p256)
  ratio_of_frozen_deposits_slashed_per_double_endorsement:
    seq:
    - id: numerator
      type: u2
    - id: denominator
      type: u2
  round_robin_over_delegates:
    seq:
    - id: len_round_robin_over_delegates
      type: s4
    - id: round_robin_over_delegates
      type: round_robin_over_delegates_entries
      size: len_round_robin_over_delegates
      repeat: eos
  round_robin_over_delegates_elt_entries:
    seq:
    - id: signature__v0__public_key
      type: public_key
  round_robin_over_delegates_entries:
    seq:
    - id: len_round_robin_over_delegates_elt
      type: s4
    - id: round_robin_over_delegates_elt
      type: round_robin_over_delegates_elt_entries
      size: len_round_robin_over_delegates_elt
      repeat: eos
  z:
    seq:
    - id: has_tail
      type: b1be
    - id: sign
      type: b1be
    - id: payload
      type: b6be
    - id: tail
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
      if: has_tail.as<bool>
enums:
  delegate_selection_tag:
    0: random_delegate_selection
    1: round_robin_over_delegates
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
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
  type: cache_layout
- id: michelson_maximum_type_size
  type: u2
- id: preserved_cycles
  type: u1
- id: blocks_per_cycle
  type: s4
- id: blocks_per_commitment
  type: s4
- id: blocks_per_stake_snapshot
  type: s4
- id: blocks_per_voting_period
  type: s4
- id: hard_gas_limit_per_operation
  type: z
- id: hard_gas_limit_per_block
  type: z
- id: proof_of_work_threshold
  type: s8
- id: tokens_per_roll
  type: n
- id: seed_nonce_revelation_tip
  type: n
- id: origination_size
  type: s4
- id: baking_reward_fixed_portion
  type: n
- id: baking_reward_bonus_per_slot
  type: n
- id: endorsing_reward_per_slot
  type: n
- id: cost_per_byte
  type: n
- id: hard_storage_limit_per_operation
  type: z
- id: quorum_min
  type: s4
- id: quorum_max
  type: s4
- id: min_proposal_quorum
  type: s4
- id: liquidity_baking_subsidy
  type: n
- id: liquidity_baking_sunset_level
  type: s4
- id: liquidity_baking_escape_ema_threshold
  type: s4
- id: max_operations_time_to_live
  type: s2
- id: minimal_block_delay
  type: s8
- id: delay_increment_per_round
  type: s8
- id: consensus_committee_size
  type: s4
- id: consensus_threshold
  type: s4
- id: minimal_participation_ratio
  type: minimal_participation_ratio
- id: max_slashing_period
  type: s4
- id: frozen_deposits_percentage
  type: s4
- id: double_baking_punishment
  type: n
- id: ratio_of_frozen_deposits_slashed_per_double_endorsement
  type: ratio_of_frozen_deposits_slashed_per_double_endorsement
- id: delegate_selection
  type: delegate_selection
