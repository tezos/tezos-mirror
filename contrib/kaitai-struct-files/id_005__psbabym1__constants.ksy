meta:
  id: id_005__psbabym1__constants
  endian: be
doc: ! 'Encoding id: 005-PsBabyM1.constants'
types:
  id_005__psbabym1__mutez:
    seq:
    - id: id_005__psbabym1__mutez
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
- id: proof_of_work_nonce_size
  type: u1
- id: nonce_length
  type: u1
- id: max_revelations_per_block
  type: u1
- id: max_operation_data_length
  type: s4
- id: max_proposals_per_delegate
  type: u1
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
  type: id_005__psbabym1__mutez
- id: michelson_maximum_type_size
  type: u2
- id: seed_nonce_revelation_tip
  type: id_005__psbabym1__mutez
- id: origination_size
  type: s4
- id: block_security_deposit
  type: id_005__psbabym1__mutez
- id: endorsement_security_deposit
  type: id_005__psbabym1__mutez
- id: block_reward
  type: id_005__psbabym1__mutez
- id: endorsement_reward
  type: id_005__psbabym1__mutez
- id: cost_per_byte
  type: id_005__psbabym1__mutez
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
