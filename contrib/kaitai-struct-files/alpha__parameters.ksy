meta:
  id: alpha__parameters
  endian: be
types:
  adaptive_rewards_params:
    seq:
    - id: issuance_ratio_min
      type: issuance_ratio_min
    - id: issuance_ratio_max
      type: issuance_ratio_max
    - id: max_bonus
      type: s8
    - id: growth_rate
      type: growth_rate
    - id: center_dz
      type: center_dz
    - id: radius_dz
      type: radius_dz
  radius_dz:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  center_dz:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  growth_rate:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  issuance_ratio_max:
    seq:
    - id: numerator
      type: z
    - id: denominator
      type: z
  issuance_ratio_min:
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
  dal_parametric:
    seq:
    - id: feature_enable
      type: u1
      enum: bool
    - id: number_of_slots
      type: s2
    - id: attestation_lag
      type: s2
    - id: attestation_threshold
      type: s2
    - id: blocks_per_epoch
      type: s4
    - id: redundancy_factor
      type: u1
    - id: page_size
      type: u2
    - id: slot_size
      type: s4
    - id: number_of_shards
      type: u2
  minimal_participation_ratio:
    seq:
    - id: numerator
      type: u2
    - id: denominator
      type: u2
  issuance_weights:
    seq:
    - id: base_total_issued_per_minute
      type: n
    - id: baking_reward_fixed_portion_weight
      type: s4
    - id: baking_reward_bonus_weight
      type: s4
    - id: attesting_reward_weight
      type: s4
    - id: liquidity_baking_subsidy_weight
      type: s4
    - id: seed_nonce_revelation_tip_weight
      type: s4
    - id: vdf_revelation_tip_weight
      type: s4
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
  commitments:
    seq:
    - id: len_commitments
      type: s4
    - id: commitments
      type: commitments_entries
      size: len_commitments
      repeat: eos
  commitments_entries:
    seq:
    - id: commitments_elt_field0
      size: 20
      doc: blinded__public__key__hash
    - id: commitments_elt_field1
      type: n
      doc: alpha__mutez
  bootstrap_smart_rollups:
    seq:
    - id: len_bootstrap_smart_rollups
      type: s4
    - id: bootstrap_smart_rollups
      type: bootstrap_smart_rollups_entries
      size: len_bootstrap_smart_rollups
      repeat: eos
  bootstrap_smart_rollups_entries:
    seq:
    - id: address
      size: 20
    - id: pvm_kind
      type: u1
      enum: pvm_kind
    - id: kernel
      type: kernel
    - id: parameters_ty
      type: parameters_ty
    - id: whitelist_tag
      type: u1
      enum: bool
    - id: whitelist
      type: whitelist
      if: (whitelist_tag == bool::true)
  whitelist:
    seq:
    - id: len_whitelist
      type: s4
    - id: whitelist
      type: whitelist_entries
      size: len_whitelist
      repeat: eos
  whitelist_entries:
    seq:
    - id: signature__public_key_hash
      type: public_key_hash
  parameters_ty:
    seq:
    - id: len_parameters_ty
      type: s4
    - id: parameters_ty
      size: len_parameters_ty
  kernel:
    seq:
    - id: len_kernel
      type: s4
    - id: kernel
      size: len_kernel
  bootstrap_contracts:
    seq:
    - id: len_bootstrap_contracts
      type: s4
    - id: bootstrap_contracts
      type: bootstrap_contracts_entries
      size: len_bootstrap_contracts
      repeat: eos
  bootstrap_contracts_entries:
    seq:
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
    - id: amount
      type: n
    - id: script
      type: alpha__scripted__contracts
    - id: hash_tag
      type: u1
      enum: bool
    - id: hash
      size: 20
      if: (hash_tag == bool::true)
  alpha__scripted__contracts:
    seq:
    - id: code
      type: code
    - id: storage
      type: storage
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  bootstrap_accounts:
    seq:
    - id: len_bootstrap_accounts
      type: s4
    - id: bootstrap_accounts
      type: bootstrap_accounts_entries
      size: len_bootstrap_accounts
      repeat: eos
  bootstrap_accounts_entries:
    seq:
    - id: bootstrap_accounts_elt_tag
      type: u1
      enum: bootstrap_accounts_elt_tag
    - id: bootstrap_accounts_elt_public_key_known
      type: bootstrap_accounts_elt_public_key_known
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_known)
    - id: bootstrap_accounts_elt_public_key_unknown
      type: bootstrap_accounts_elt_public_key_unknown
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_unknown)
    - id: bootstrap_accounts_elt_public_key_known_with_delegate
      type: bootstrap_accounts_elt_public_key_known_with_delegate
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_known_with_delegate)
    - id: bootstrap_accounts_elt_public_key_unknown_with_delegate
      type: bootstrap_accounts_elt_public_key_unknown_with_delegate
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_unknown_with_delegate)
    - id: bootstrap_accounts_elt_public_key_known_with_consensus_key
      type: bootstrap_accounts_elt_public_key_known_with_consensus_key
      if: (bootstrap_accounts_elt_tag == bootstrap_accounts_elt_tag::public_key_known_with_consensus_key)
  bootstrap_accounts_elt_public_key_known_with_consensus_key:
    seq:
    - id: public_key_known_with_consensus_key_field0
      type: public_key
      doc: signature__public_key
    - id: public_key_known_with_consensus_key_field1
      type: n
      doc: alpha__mutez
    - id: public_key_known_with_consensus_key_field2
      type: public_key
      doc: signature__public_key
  bootstrap_accounts_elt_public_key_unknown_with_delegate:
    seq:
    - id: public_key_unknown_with_delegate_field0
      type: public_key_hash
      doc: signature__public_key_hash
    - id: public_key_unknown_with_delegate_field1
      type: n
      doc: alpha__mutez
    - id: public_key_unknown_with_delegate_field2
      type: public_key_hash
      doc: signature__public_key_hash
  bootstrap_accounts_elt_public_key_known_with_delegate:
    seq:
    - id: public_key_known_with_delegate_field0
      type: public_key
      doc: signature__public_key
    - id: public_key_known_with_delegate_field1
      type: n
      doc: alpha__mutez
    - id: public_key_known_with_delegate_field2
      type: public_key_hash
      doc: signature__public_key_hash
  bootstrap_accounts_elt_public_key_unknown:
    seq:
    - id: public_key_unknown_field0
      type: public_key_hash
      doc: signature__public_key_hash
    - id: public_key_unknown_field1
      type: n
      doc: alpha__mutez
  public_key_hash:
    doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: public_key_hash_ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: public_key_hash_secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: public_key_hash_p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
    - id: public_key_hash_bls
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::bls)
  bootstrap_accounts_elt_public_key_known:
    seq:
    - id: public_key_known_field0
      type: public_key
      doc: signature__public_key
    - id: public_key_known_field1
      type: n
      doc: alpha__mutez
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
    - id: public_key_ed25519
      size: 32
      if: (public_key_tag == public_key_tag::ed25519)
    - id: public_key_secp256k1
      size: 33
      if: (public_key_tag == public_key_tag::secp256k1)
    - id: public_key_p256
      size: 33
      if: (public_key_tag == public_key_tag::p256)
    - id: public_key_bls
      size: 48
      if: (public_key_tag == public_key_tag::bls)
enums:
  pvm_kind:
    0: arith
    1: wasm_2_0_0
    2: riscv
  bool:
    0: false
    255: true
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
  bootstrap_accounts_elt_tag:
    0: public_key_known
    1: public_key_unknown
    2: public_key_known_with_delegate
    3: public_key_unknown_with_delegate
    4: public_key_known_with_consensus_key
seq:
- id: bootstrap_accounts
  type: bootstrap_accounts
- id: bootstrap_contracts
  type: bootstrap_contracts
- id: bootstrap_smart_rollups
  type: bootstrap_smart_rollups
- id: commitments
  type: commitments
- id: security_deposit_ramp_up_cycles_tag
  type: u1
  enum: bool
- id: security_deposit_ramp_up_cycles
  type: s4
  if: (security_deposit_ramp_up_cycles_tag == bool::true)
- id: no_reward_cycles_tag
  type: u1
  enum: bool
- id: no_reward_cycles
  type: s4
  if: (no_reward_cycles_tag == bool::true)
- id: preserved_cycles
  type: u1
- id: blocks_per_cycle
  type: s4
- id: blocks_per_commitment
  type: s4
- id: nonce_revelation_threshold
  type: s4
- id: blocks_per_stake_snapshot
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
  type: n
- id: minimal_frozen_stake
  type: n
- id: vdf_difficulty
  type: s8
- id: origination_size
  type: s4
- id: issuance_weights
  type: issuance_weights
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
- id: liquidity_baking_toggle_ema_threshold
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
- id: limit_of_delegation_over_baking
  type: u1
- id: percentage_of_frozen_deposits_slashed_per_double_baking
  type: u1
- id: percentage_of_frozen_deposits_slashed_per_double_attestation
  type: u1
- id: testnet_dictator_tag
  type: u1
  enum: bool
- id: testnet_dictator
  type: public_key_hash
  if: (testnet_dictator_tag == bool::true)
- id: initial_seed_tag
  type: u1
  enum: bool
- id: initial_seed
  size: 32
  if: (initial_seed_tag == bool::true)
- id: cache_script_size
  type: s4
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
  type: s4
- id: smart_rollup_challenge_window_in_blocks
  type: s4
- id: smart_rollup_stake_amount
  type: n
- id: smart_rollup_commitment_period_in_blocks
  type: s4
- id: smart_rollup_max_lookahead_in_blocks
  type: s4
- id: smart_rollup_max_active_outbox_levels
  type: s4
- id: smart_rollup_max_outbox_messages_per_level
  type: s4
- id: smart_rollup_number_of_sections_in_dissection
  type: u1
- id: smart_rollup_timeout_period_in_blocks
  type: s4
- id: smart_rollup_max_number_of_cemented_commitments
  type: s4
- id: smart_rollup_max_number_of_parallel_games
  type: s4
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
  type: s4
- id: zk_rollup_min_pending_to_process
  type: s4
- id: zk_rollup_max_ticket_payload_size
  type: s4
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
