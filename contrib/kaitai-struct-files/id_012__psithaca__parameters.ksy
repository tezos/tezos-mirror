meta:
  id: id_012__psithaca__parameters
  endian: be
doc: ! 'Encoding id: 012-Psithaca.parameters'
types:
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
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: amount
      type: id_012__psithaca__mutez
    - id: script
      type: id_012__psithaca__scripted__contracts
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
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
      type: id_012__psithaca__mutez
      doc: id_012__psithaca__mutez
  delegate_selection:
    seq:
    - id: delegate_selection_tag
      type: u1
      enum: delegate_selection_tag
    - id: round_robin_over_delegates
      type: round_robin_over_delegates_0
      if: (delegate_selection_tag == delegate_selection_tag::round_robin_over_delegates)
  id_012__psithaca__mutez:
    seq:
    - id: id_012__psithaca__mutez
      type: n
  id_012__psithaca__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  int31:
    seq:
    - id: int31
      type: s4
      valid:
        min: -1073741824
        max: 1073741823
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
  public_key_known:
    seq:
    - id: public_key_known_field0
      type: public_key
      doc: ! 'A Ed25519, Secp256k1, or P256 public key


        signature__v0__public_key'
    - id: public_key_known_field1
      type: id_012__psithaca__mutez
      doc: id_012__psithaca__mutez
  public_key_unknown:
    seq:
    - id: public_key_unknown_field0
      type: public_key_hash
      doc: ! 'A Ed25519, Secp256k1, or P256 public key hash


        signature__v0__public_key_hash'
    - id: public_key_unknown_field1
      type: id_012__psithaca__mutez
      doc: id_012__psithaca__mutez
  ratio_of_frozen_deposits_slashed_per_double_endorsement:
    seq:
    - id: numerator
      type: u2
    - id: denominator
      type: u2
  round_robin_over_delegates:
    seq:
    - id: round_robin_over_delegates_entries
      type: round_robin_over_delegates_entries
      repeat: eos
  round_robin_over_delegates_0:
    seq:
    - id: len_round_robin_over_delegates
      type: u4
      valid:
        max: 1073741823
    - id: round_robin_over_delegates
      type: round_robin_over_delegates
      size: len_round_robin_over_delegates
  round_robin_over_delegates_elt:
    seq:
    - id: round_robin_over_delegates_elt_entries
      type: round_robin_over_delegates_elt_entries
      repeat: eos
  round_robin_over_delegates_elt_entries:
    seq:
    - id: signature__v0__public_key
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
  round_robin_over_delegates_entries:
    seq:
    - id: len_round_robin_over_delegates_elt
      type: u4
      valid:
        max: 1073741823
    - id: round_robin_over_delegates_elt
      type: round_robin_over_delegates_elt
      size: len_round_robin_over_delegates_elt
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
  delegate_selection_tag:
    0: random_delegate_selection
    1: round_robin_over_delegates
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
  type: bootstrap_accounts_0
- id: bootstrap_contracts
  type: bootstrap_contracts_0
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
  type: id_012__psithaca__mutez
- id: seed_nonce_revelation_tip
  type: id_012__psithaca__mutez
- id: origination_size
  type: int31
- id: baking_reward_fixed_portion
  type: id_012__psithaca__mutez
- id: baking_reward_bonus_per_slot
  type: id_012__psithaca__mutez
- id: endorsing_reward_per_slot
  type: id_012__psithaca__mutez
- id: cost_per_byte
  type: id_012__psithaca__mutez
- id: hard_storage_limit_per_operation
  type: z
- id: quorum_min
  type: s4
- id: quorum_max
  type: s4
- id: min_proposal_quorum
  type: s4
- id: liquidity_baking_subsidy
  type: id_012__psithaca__mutez
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
  type: int31
- id: consensus_threshold
  type: int31
- id: minimal_participation_ratio
  type: minimal_participation_ratio
- id: max_slashing_period
  type: int31
- id: frozen_deposits_percentage
  type: int31
- id: double_baking_punishment
  type: id_012__psithaca__mutez
- id: ratio_of_frozen_deposits_slashed_per_double_endorsement
  type: ratio_of_frozen_deposits_slashed_per_double_endorsement
- id: delegate_selection
  type: delegate_selection
