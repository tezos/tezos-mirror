meta:
  id: id_011__pthangz2__constants__parametric
  endian: be
doc: ! 'Encoding id: 011-PtHangz2.constants.parametric'
types:
  baking_reward_per_endorsement:
    seq:
    - id: len_baking_reward_per_endorsement
      type: s4
    - id: baking_reward_per_endorsement
      type: baking_reward_per_endorsement_entries
      size: len_baking_reward_per_endorsement
      repeat: eos
  baking_reward_per_endorsement_entries:
    seq:
    - id: id_011__pthangz2__mutez
      type: n
  endorsement_reward:
    seq:
    - id: len_endorsement_reward
      type: s4
    - id: endorsement_reward
      type: endorsement_reward_entries
      size: len_endorsement_reward
      repeat: eos
  endorsement_reward_entries:
    seq:
    - id: id_011__pthangz2__mutez
      type: n
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
  time_between_blocks:
    seq:
    - id: len_time_between_blocks
      type: s4
    - id: time_between_blocks
      type: time_between_blocks_entries
      size: len_time_between_blocks
      repeat: eos
  time_between_blocks_entries:
    seq:
    - id: time_between_blocks_elt
      type: s8
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
seq:
- id: preserved_cycles
  type: u1
- id: blocks_per_cycle
  type: s4
- id: blocks_per_commitment
  type: s4
- id: blocks_per_roll_snapshot
  type: s4
- id: blocks_per_voting_period
  type: s4
- id: time_between_blocks
  type: time_between_blocks
- id: endorsers_per_block
  type: u2
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
- id: block_security_deposit
  type: n
- id: endorsement_security_deposit
  type: n
- id: baking_reward_per_endorsement
  type: baking_reward_per_endorsement
- id: endorsement_reward
  type: endorsement_reward
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
- id: initial_endorsers
  type: u2
- id: delay_per_missing_endorsement
  type: s8
- id: minimal_block_delay
  type: s8
- id: liquidity_baking_subsidy
  type: n
- id: liquidity_baking_sunset_level
  type: s4
- id: liquidity_baking_escape_ema_threshold
  type: s4
