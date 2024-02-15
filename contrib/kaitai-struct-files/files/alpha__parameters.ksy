meta:
  id: alpha__parameters
  endian: be
doc: ! 'Encoding id: alpha.parameters'
types:
  adaptive_rewards_params:
    seq:
    - id: issuance_ratio_final_min
      type: issuance_ratio_final_min
    - id: issuance_ratio_final_max
      type: issuance_ratio_final_max
    - id: issuance_ratio_initial_min
      type: issuance_ratio_initial_min
    - id: issuance_ratio_initial_max
      type: issuance_ratio_initial_max
    - id: initial_period
      type: u1
    - id: transition_period
      type: u1
    - id: max_bonus
      type: s8
    - id: growth_rate
      type: growth_rate
    - id: center_dz
      type: center_dz
    - id: radius_dz
      type: radius_dz
  alpha__mutez:
    seq:
    - id: alpha__mutez
      type: n
  alpha__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  bootstrap_accounts:
    seq:
    - id: bootstrap_accounts_entries
      type: bootstrap_accounts_entries
      repeat: eos
  bootstrap_accounts_0:
    seq:
    - id: len_bootstrap_accounts
      type: u4
      valid:
        max: 1073741823
    - id: bootstrap_accounts
      type: bootstrap_accounts
      size: len_bootstrap_accounts
  bootstrap_accounts_entries:
    seq:
    - id: bootstrap_accounts_elt_tag
      type: u1
      enum: bootstrap_accounts_elt_tag
    - id: public_key_known
      type: public_key_known
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_known)
    - id: public_key_unknown
      type: public_key_unknown
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_unknown)
    - id: public_key_known_with_delegate
      type: public_key_known_with_delegate
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_known_with_delegate)
    - id: public_key_unknown_with_delegate
      type: public_key_unknown_with_delegate
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_unknown_with_delegate)
    - id: public_key_known_with_consensus_key
      type: public_key_known_with_consensus_key
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_known_with_consensus_key)
  bootstrap_contracts:
    seq:
    - id: bootstrap_contracts_entries
      type: bootstrap_contracts_entries
      repeat: eos
  bootstrap_contracts_0:
    seq:
    - id: len_bootstrap_contracts
      type: u4
      valid:
        max: 1073741823
    - id: bootstrap_contracts
      type: bootstrap_contracts
      size: len_bootstrap_contracts
  bootstrap_contracts_entries:
    seq:
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: amount
      type: alpha__mutez
    - id: script
      type: alpha__scripted__contracts
    - id: hash_tag
      type: u1
      enum: bool
    - id: hash
      size: 20
      if: (hash_tag == bool::true)
  bootstrap_smart_rollups:
    seq:
    - id: bootstrap_smart_rollups_entries
      type: bootstrap_smart_rollups_entries
      repeat: eos
  bootstrap_smart_rollups_0:
    seq:
    - id: len_bootstrap_smart_rollups
      type: u4
      valid:
        max: 1073741823
    - id: bootstrap_smart_rollups
      type: bootstrap_smart_rollups
      size: len_bootstrap_smart_rollups
  bootstrap_smart_rollups_entries:
    seq:
    - id: address
      size: 20
    - id: pvm_kind
      type: u1
      enum: pvm_kind
    - id: kernel
      type: bytes_dyn_uint30
    - id: parameters_ty
      type: bytes_dyn_uint30
    - id: whitelist_tag
      type: u1
      enum: bool
    - id: whitelist
      type: whitelist_0
      if: (whitelist_tag == bool::true)
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  center_dz:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  commitments:
    seq:
    - id: commitments_entries
      type: commitments_entries
      repeat: eos
  commitments_0:
    seq:
    - id: len_commitments
      type: u4
      valid:
        max: 1073741823
    - id: commitments
      type: commitments
      size: len_commitments
  commitments_entries:
    seq:
    - id: commitments_elt_field0
      size: 20
      doc: blinded__public__key__hash
    - id: commitments_elt_field1
      type: alpha__mutez
      doc: alpha__mutez
  dal_parametric:
    seq:
    - id: feature_enable
      type: u1
      enum: bool
    - id: incentives_enable
      type: u1
      enum: bool
    - id: number_of_slots
      type: u2
    - id: attestation_lag
      type: u1
    - id: attestation_threshold
      type: u1
    - id: redundancy_factor
      type: u1
    - id: page_size
      type: u2
    - id: slot_size
      type: int31
    - id: number_of_shards
      type: u2
  growth_rate:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  int31:
    seq:
    - id: int31
      type: s4
      valid:
        min: -1073741824
        max: 1073741823
  issuance_ratio_final_max:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  issuance_ratio_final_min:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  issuance_ratio_initial_max:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  issuance_ratio_initial_min:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  issuance_weights:
    seq:
    - id: base_total_issued_per_minute
      type: alpha__mutez
    - id: baking_reward_fixed_portion_weight
      type: int31
    - id: baking_reward_bonus_weight
      type: int31
    - id: attesting_reward_weight
      type: int31
    - id: liquidity_baking_subsidy_weight
      type: int31
    - id: seed_nonce_revelation_tip_weight
      type: int31
    - id: vdf_revelation_tip_weight
      type: int31
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
    - id: bls
      size: 48
      if: (public_key_tag == public_key_tag::bls)
  public_key_hash:
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
    - id: bls
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::bls)
  public_key_known:
    seq:
    - id: public_key_known_field0
      type: public_key
      doc: ! 'A Ed25519, Secp256k1, or P256 public key


        signature__public_key'
    - id: public_key_known_field1
      type: alpha__mutez
      doc: alpha__mutez
  public_key_known_with_consensus_key:
    seq:
    - id: public_key_known_with_consensus_key_field0
      type: public_key
      doc: ! 'A Ed25519, Secp256k1, or P256 public key


        signature__public_key'
    - id: public_key_known_with_consensus_key_field1
      type: alpha__mutez
      doc: alpha__mutez
    - id: public_key_known_with_consensus_key_field2
      type: public_key
      doc: ! 'A Ed25519, Secp256k1, or P256 public key


        signature__public_key'
  public_key_known_with_delegate:
    seq:
    - id: public_key_known_with_delegate_field0
      type: public_key
      doc: ! 'A Ed25519, Secp256k1, or P256 public key


        signature__public_key'
    - id: public_key_known_with_delegate_field1
      type: alpha__mutez
      doc: alpha__mutez
    - id: public_key_known_with_delegate_field2
      type: public_key_hash
      doc: ! 'A Ed25519, Secp256k1, P256, or BLS public key hash


        signature__public_key_hash'
  public_key_unknown:
    seq:
    - id: public_key_unknown_field0
      type: public_key_hash
      doc: ! 'A Ed25519, Secp256k1, P256, or BLS public key hash


        signature__public_key_hash'
    - id: public_key_unknown_field1
      type: alpha__mutez
      doc: alpha__mutez
  public_key_unknown_with_delegate:
    seq:
    - id: public_key_unknown_with_delegate_field0
      type: public_key_hash
      doc: ! 'A Ed25519, Secp256k1, P256, or BLS public key hash


        signature__public_key_hash'
    - id: public_key_unknown_with_delegate_field1
      type: alpha__mutez
      doc: alpha__mutez
    - id: public_key_unknown_with_delegate_field2
      type: public_key_hash
      doc: ! 'A Ed25519, Secp256k1, P256, or BLS public key hash


        signature__public_key_hash'
  radius_dz:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  smart_rollup_reveal_activation_level:
    seq:
    - id: raw_data
      type: s4
    - id: metadata
      type: s4
    - id: dal_page
      type: s4
    - id: dal_parameters
      type: s4
  whitelist:
    seq:
    - id: whitelist_entries
      type: whitelist_entries
      repeat: eos
  whitelist_0:
    seq:
    - id: len_whitelist
      type: u4
      valid:
        max: 1073741823
    - id: whitelist
      type: whitelist
      size: len_whitelist
  whitelist_entries:
    seq:
    - id: signature__public_key_hash
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
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
  bool:
    0: false
    255: true
  bootstrap_accounts_elt_tag:
    0: public_key_known
    1: public_key_unknown
    2: public_key_known_with_delegate
    3: public_key_unknown_with_delegate
    4: public_key_known_with_consensus_key
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  pvm_kind:
    0: arith
    1: wasm_2_0_0
    2: riscv
seq:
- id: bootstrap_accounts
  type: bootstrap_accounts_0
- id: bootstrap_contracts
  type: bootstrap_contracts_0
- id: bootstrap_smart_rollups
  type: bootstrap_smart_rollups_0
- id: commitments
  type: commitments_0
- id: security_deposit_ramp_up_cycles_tag
  type: u1
  enum: bool
- id: security_deposit_ramp_up_cycles
  type: int31
  if: (security_deposit_ramp_up_cycles_tag == bool::true)
- id: no_reward_cycles_tag
  type: u1
  enum: bool
- id: no_reward_cycles
  type: int31
  if: (no_reward_cycles_tag == bool::true)
- id: consensus_rights_delay
  type: u1
- id: blocks_preservation_cycles
  type: u1
- id: delegate_parameters_activation_delay
  type: u1
- id: blocks_per_cycle
  type: s4
- id: blocks_per_commitment
  type: s4
- id: nonce_revelation_threshold
  type: s4
- id: cycles_per_voting_period
  type: s4
- id: hard_gas_limit_per_operation
  type: z
- id: hard_gas_limit_per_block
  type: z
- id: proof_of_work_threshold
  type: s8
- id: minimal_stake
  type: alpha__mutez
- id: minimal_frozen_stake
  type: alpha__mutez
- id: vdf_difficulty
  type: s8
- id: origination_size
  type: int31
- id: issuance_weights
  type: issuance_weights
- id: cost_per_byte
  type: alpha__mutez
- id: hard_storage_limit_per_operation
  type: z
- id: quorum_min
  type: s4
- id: quorum_max
  type: s4
- id: min_proposal_quorum
  type: s4
- id: liquidity_baking_toggle_ema_threshold
  type: s4
- id: max_operations_time_to_live
  type: s2
- id: minimal_block_delay
  type: s8
- id: delay_increment_per_round
  type: s8
- id: consensus_committee_size
  type: int31
- id: consensus_threshold
  type: int31
- id: minimal_participation_ratio
  type: minimal_participation_ratio
- id: limit_of_delegation_over_baking
  type: u1
- id: percentage_of_frozen_deposits_slashed_per_double_baking
  type: u2
- id: percentage_of_frozen_deposits_slashed_per_double_attestation
  type: u2
- id: max_slashing_per_block
  type: u2
- id: max_slashing_threshold
  type: int31
- id: testnet_dictator_tag
  type: u1
  enum: bool
- id: testnet_dictator
  type: public_key_hash
  if: (testnet_dictator_tag == bool::true)
  doc: A Ed25519, Secp256k1, P256, or BLS public key hash
- id: initial_seed_tag
  type: u1
  enum: bool
- id: initial_seed
  size: 32
  if: (initial_seed_tag == bool::true)
- id: cache_script_size
  type: int31
- id: cache_stake_distribution_cycles
  type: s1
- id: cache_sampler_state_cycles
  type: s1
- id: dal_parametric
  type: dal_parametric
- id: smart_rollup_arith_pvm_enable
  type: u1
  enum: bool
- id: smart_rollup_origination_size
  type: int31
- id: smart_rollup_challenge_window_in_blocks
  type: int31
- id: smart_rollup_stake_amount
  type: alpha__mutez
- id: smart_rollup_commitment_period_in_blocks
  type: int31
- id: smart_rollup_max_lookahead_in_blocks
  type: s4
- id: smart_rollup_max_active_outbox_levels
  type: s4
- id: smart_rollup_max_outbox_messages_per_level
  type: int31
- id: smart_rollup_number_of_sections_in_dissection
  type: u1
- id: smart_rollup_timeout_period_in_blocks
  type: int31
- id: smart_rollup_max_number_of_cemented_commitments
  type: int31
- id: smart_rollup_max_number_of_parallel_games
  type: int31
- id: smart_rollup_reveal_activation_level
  type: smart_rollup_reveal_activation_level
- id: smart_rollup_private_enable
  type: u1
  enum: bool
- id: smart_rollup_riscv_pvm_enable
  type: u1
  enum: bool
- id: zk_rollup_enable
  type: u1
  enum: bool
- id: zk_rollup_origination_size
  type: int31
- id: zk_rollup_min_pending_to_process
  type: int31
- id: zk_rollup_max_ticket_payload_size
  type: int31
- id: global_limit_of_staking_over_baking
  type: u1
- id: edge_of_staking_over_delegation
  type: u1
- id: adaptive_issuance_launch_ema_threshold
  type: s4
- id: adaptive_rewards_params
  type: adaptive_rewards_params
- id: adaptive_issuance_activation_vote_enable
  type: u1
  enum: bool
- id: autostaking_enable
  type: u1
  enum: bool
- id: adaptive_issuance_force_activation
  type: u1
  enum: bool
- id: ns_enable
  type: u1
  enum: bool
- id: direct_ticket_spending_enable
  type: u1
  enum: bool
