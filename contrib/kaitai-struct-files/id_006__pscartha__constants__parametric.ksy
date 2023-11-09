meta:
  id: id_006__pscartha__constants__parametric
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.constants.parametric'
types:
  baking_reward_per_endorsement:
    seq:
    - id: baking_reward_per_endorsement_entries
      type: baking_reward_per_endorsement_entries
      repeat: eos
  baking_reward_per_endorsement_0:
    seq:
    - id: len_baking_reward_per_endorsement
      type: s4
    - id: baking_reward_per_endorsement
      type: baking_reward_per_endorsement
      size: len_baking_reward_per_endorsement
  baking_reward_per_endorsement_entries:
    seq:
    - id: id_006__pscartha__mutez
      type: id_006__pscartha__mutez
  endorsement_reward:
    seq:
    - id: endorsement_reward_entries
      type: endorsement_reward_entries
      repeat: eos
  endorsement_reward_0:
    seq:
    - id: len_endorsement_reward
      type: s4
    - id: endorsement_reward
      type: endorsement_reward
      size: len_endorsement_reward
  endorsement_reward_entries:
    seq:
    - id: id_006__pscartha__mutez
      type: id_006__pscartha__mutez
  id_006__pscartha__mutez:
    seq:
    - id: id_006__pscartha__mutez
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
    - id: time_between_blocks_entries
      type: time_between_blocks_entries
      repeat: eos
  time_between_blocks_0:
    seq:
    - id: len_time_between_blocks
      type: s4
    - id: time_between_blocks
      type: time_between_blocks
      size: len_time_between_blocks
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
  type: time_between_blocks_0
- id: endorsers_per_block
  type: u2
- id: hard_gas_limit_per_operation
  type: z
- id: hard_gas_limit_per_block
  type: z
- id: proof_of_work_threshold
  type: s8
- id: tokens_per_roll
  type: id_006__pscartha__mutez
- id: michelson_maximum_type_size
  type: u2
- id: seed_nonce_revelation_tip
  type: id_006__pscartha__mutez
- id: origination_size
  type: s4
- id: block_security_deposit
  type: id_006__pscartha__mutez
- id: endorsement_security_deposit
  type: id_006__pscartha__mutez
- id: baking_reward_per_endorsement
  type: baking_reward_per_endorsement_0
- id: endorsement_reward
  type: endorsement_reward_0
- id: cost_per_byte
  type: id_006__pscartha__mutez
- id: hard_storage_limit_per_operation
  type: z
- id: test_chain_duration
  type: s8
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
