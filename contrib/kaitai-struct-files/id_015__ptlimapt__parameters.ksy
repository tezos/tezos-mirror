meta:
  id: id_015__ptlimapt__parameters
  endian: be
doc: ! 'Encoding id: 015-PtLimaPt.parameters'
types:
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
      type: id_015__ptlimapt__scripted__contracts
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
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
      doc: id_015__ptlimapt__mutez
  dal_parametric:
    seq:
    - id: feature_enable
      type: u1
      enum: bool
    - id: number_of_slots
      type: s2
    - id: number_of_shards
      type: s2
    - id: endorsement_lag
      type: s2
    - id: availability_threshold
      type: s2
    - id: slot_size
      type: s4
    - id: redundancy_factor
      type: u1
    - id: page_size
      type: u2
  id_015__ptlimapt__scripted__contracts:
    seq:
    - id: code
      type: code
    - id: storage
      type: storage
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
  public_key_hash:
    doc: A Ed25519, Secp256k1, or P256 public key hash
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
  public_key_known:
    seq:
    - id: public_key_known_field0
      type: public_key
      doc: signature__v0__public_key
    - id: public_key_known_field1
      type: n
      doc: id_015__ptlimapt__mutez
  public_key_known_with_consensus_key:
    seq:
    - id: public_key_known_with_consensus_key_field0
      type: public_key
      doc: signature__v0__public_key
    - id: public_key_known_with_consensus_key_field1
      type: n
      doc: id_015__ptlimapt__mutez
    - id: public_key_known_with_consensus_key_field2
      type: public_key
      doc: signature__v0__public_key
  public_key_known_with_delegate:
    seq:
    - id: public_key_known_with_delegate_field0
      type: public_key
      doc: signature__v0__public_key
    - id: public_key_known_with_delegate_field1
      type: n
      doc: id_015__ptlimapt__mutez
    - id: public_key_known_with_delegate_field2
      type: public_key_hash
      doc: signature__v0__public_key_hash
  public_key_unknown:
    seq:
    - id: public_key_unknown_field0
      type: public_key_hash
      doc: signature__v0__public_key_hash
    - id: public_key_unknown_field1
      type: n
      doc: id_015__ptlimapt__mutez
  public_key_unknown_with_delegate:
    seq:
    - id: public_key_unknown_with_delegate_field0
      type: public_key_hash
      doc: signature__v0__public_key_hash
    - id: public_key_unknown_with_delegate_field1
      type: n
      doc: id_015__ptlimapt__mutez
    - id: public_key_unknown_with_delegate_field2
      type: public_key_hash
      doc: signature__v0__public_key_hash
  ratio_of_frozen_deposits_slashed_per_double_endorsement:
    seq:
    - id: numerator
      type: u2
    - id: denominator
      type: u2
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
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
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: bootstrap_accounts
  type: bootstrap_accounts
- id: bootstrap_contracts
  type: bootstrap_contracts
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
- id: vdf_difficulty
  type: s8
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
- id: max_slashing_period
  type: s4
- id: frozen_deposits_percentage
  type: s4
- id: double_baking_punishment
  type: n
- id: ratio_of_frozen_deposits_slashed_per_double_endorsement
  type: ratio_of_frozen_deposits_slashed_per_double_endorsement
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
- id: tx_rollup_enable
  type: u1
  enum: bool
- id: tx_rollup_origination_size
  type: s4
- id: tx_rollup_hard_size_limit_per_inbox
  type: s4
- id: tx_rollup_hard_size_limit_per_message
  type: s4
- id: tx_rollup_max_withdrawals_per_batch
  type: s4
- id: tx_rollup_commitment_bond
  type: n
- id: tx_rollup_finality_period
  type: s4
- id: tx_rollup_withdraw_period
  type: s4
- id: tx_rollup_max_inboxes_count
  type: s4
- id: tx_rollup_max_messages_per_inbox
  type: s4
- id: tx_rollup_max_commitments_count
  type: s4
- id: tx_rollup_cost_per_byte_ema_factor
  type: s4
- id: tx_rollup_max_ticket_payload_size
  type: s4
- id: tx_rollup_rejection_max_proof_size
  type: s4
- id: tx_rollup_sunset_level
  type: s4
- id: dal_parametric
  type: dal_parametric
- id: sc_rollup_enable
  type: u1
  enum: bool
- id: sc_rollup_origination_size
  type: s4
- id: sc_rollup_challenge_window_in_blocks
  type: s4
- id: sc_rollup_max_number_of_messages_per_commitment_period
  type: s4
- id: sc_rollup_stake_amount
  type: n
- id: sc_rollup_commitment_period_in_blocks
  type: s4
- id: sc_rollup_max_lookahead_in_blocks
  type: s4
- id: sc_rollup_max_active_outbox_levels
  type: s4
- id: sc_rollup_max_outbox_messages_per_level
  type: s4
- id: sc_rollup_number_of_sections_in_dissection
  type: u1
- id: sc_rollup_timeout_period_in_blocks
  type: s4
- id: sc_rollup_max_number_of_cemented_commitments
  type: s4
- id: zk_rollup_enable
  type: u1
  enum: bool
- id: zk_rollup_origination_size
  type: s4
- id: zk_rollup_min_pending_to_process
  type: s4
