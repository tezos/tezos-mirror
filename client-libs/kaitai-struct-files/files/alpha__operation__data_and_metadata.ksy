meta:
  id: alpha__operation__data_and_metadata
  endian: be
  imports:
  - alpha__lazy_storage_diff
  - block_header__shell
  - operation__shell_header
doc: ! 'Encoding id: alpha.operation.data_and_metadata'
types:
  activate_account:
    seq:
    - id: pkh
      size: 20
    - id: secret
      size: 20
    - id: metadata
      type: alpha__operation_metadata__alpha__balance_updates_0
  activate_account_0:
    seq:
    - id: pkh
      size: 20
    - id: secret
      size: 20
  address_registry_diff:
    seq:
    - id: address_registry_diff_entries
      type: address_registry_diff_entries
      repeat: eos
  address_registry_diff_0:
    seq:
    - id: len_address_registry_diff
      type: u4be
      valid:
        max: 1073741823
    - id: address_registry_diff
      type: address_registry_diff
      size: len_address_registry_diff
  address_registry_diff_entries:
    seq:
    - id: address
      type: alpha__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: index
      type: z
  alpha__apply_internal_results__alpha__operation_result:
    seq:
    - id: alpha__apply_internal_results__alpha__operation_result_tag
      type: u1
      enum: alpha__apply_internal_results__alpha__operation_result_tag
    - id: transaction
      type: transaction
      if: (alpha__apply_internal_results__alpha__operation_result_tag == alpha__apply_internal_results__alpha__operation_result_tag::transaction)
    - id: origination
      type: origination
      if: (alpha__apply_internal_results__alpha__operation_result_tag == alpha__apply_internal_results__alpha__operation_result_tag::origination)
    - id: delegation
      type: delegation
      if: (alpha__apply_internal_results__alpha__operation_result_tag == alpha__apply_internal_results__alpha__operation_result_tag::delegation)
    - id: event
      type: event
      if: (alpha__apply_internal_results__alpha__operation_result_tag == alpha__apply_internal_results__alpha__operation_result_tag::event)
  alpha__block_header__alpha__full_header:
    seq:
    - id: alpha__block_header__alpha__full_header
      type: block_header__shell
    - id: alpha__block_header__alpha__signed_contents
      type: alpha__block_header__alpha__signed_contents
  alpha__block_header__alpha__signed_contents:
    seq:
    - id: alpha__block_header__alpha__unsigned_contents
      type: alpha__block_header__alpha__unsigned_contents
    - id: signature
      size-eos: true
  alpha__block_header__alpha__unsigned_contents:
    seq:
    - id: payload_hash
      size: 32
    - id: payload_round
      type: s4be
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
    - id: per_block_votes
      type: alpha__per_block_votes
  alpha__bond_id:
    seq:
    - id: alpha__bond_id_tag
      type: u1
      enum: alpha__bond_id_tag
    - id: smart_rollup_bond_id
      size: 20
      if: (alpha__bond_id_tag == alpha__bond_id_tag::smart_rollup_bond_id)
  alpha__contract_id:
    seq:
    - id: alpha__contract_id_tag
      type: u1
      enum: alpha__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (alpha__contract_id_tag == alpha__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (alpha__contract_id_tag == alpha__contract_id_tag::originated)
  alpha__contract_id__originated:
    seq:
    - id: alpha__contract_id__originated_tag
      type: u1
      enum: alpha__contract_id__originated_tag
    - id: originated
      type: originated
      if: (alpha__contract_id__originated_tag == alpha__contract_id__originated_tag::originated)
  alpha__entrypoint:
    seq:
    - id: alpha__entrypoint_tag
      type: u1
      enum: alpha__entrypoint_tag
    - id: named
      type: named_0
      if: (alpha__entrypoint_tag == alpha__entrypoint_tag::named)
  alpha__error:
    seq:
    - id: alpha__error
      type: bytes_dyn_uint30
  alpha__frozen_staker:
    seq:
    - id: alpha__frozen_staker_tag
      type: u1
      enum: alpha__frozen_staker_tag
    - id: single
      type: single
      if: (alpha__frozen_staker_tag == alpha__frozen_staker_tag::single)
    - id: shared
      type: public_key_hash
      if: (alpha__frozen_staker_tag == alpha__frozen_staker_tag::shared)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: baker
      type: public_key_hash
      if: (alpha__frozen_staker_tag == alpha__frozen_staker_tag::baker)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: baker_edge
      type: public_key_hash
      if: (alpha__frozen_staker_tag == alpha__frozen_staker_tag::baker_edge)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  alpha__inlined__consensus_operation:
    seq:
    - id: alpha__inlined__consensus_operation
      type: operation__shell_header
    - id: operations
      type: alpha__inlined__consensus_operation__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
  alpha__inlined__consensus_operation__contents:
    seq:
    - id: alpha__inlined__consensus_operation__contents_tag
      type: u1
      enum: alpha__inlined__consensus_operation__contents_tag
    - id: preattestation
      type: preattestation_0
      if: (alpha__inlined__consensus_operation__contents_tag == alpha__inlined__consensus_operation__contents_tag::preattestation)
    - id: attestation
      type: attestation_0
      if: (alpha__inlined__consensus_operation__contents_tag == alpha__inlined__consensus_operation__contents_tag::attestation)
    - id: attestation_with_dal
      type: attestation_with_dal_0
      if: (alpha__inlined__consensus_operation__contents_tag == alpha__inlined__consensus_operation__contents_tag::attestation_with_dal)
    - id: preattestations_aggregate
      type: preattestations_aggregate_0
      if: (alpha__inlined__consensus_operation__contents_tag == alpha__inlined__consensus_operation__contents_tag::preattestations_aggregate)
    - id: attestations_aggregate
      type: attestations_aggregate_0
      if: (alpha__inlined__consensus_operation__contents_tag == alpha__inlined__consensus_operation__contents_tag::attestations_aggregate)
  alpha__michelson__v1__primitives:
    seq:
    - id: alpha__michelson__v1__primitives
      type: u1
      enum: alpha__michelson__v1__primitives
  alpha__mutez:
    seq:
    - id: alpha__mutez
      type: n
  alpha__operation__alpha__contents:
    seq:
    - id: alpha__operation__alpha__contents_tag
      type: u1
      enum: alpha__operation__alpha__contents_tag
    - id: attestation
      type: attestation_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::attestation)
    - id: attestation_with_dal
      type: attestation_with_dal_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::attestation_with_dal)
    - id: preattestation
      type: preattestation_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::preattestation)
    - id: attestations_aggregate
      type: attestations_aggregate_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::attestations_aggregate)
    - id: preattestations_aggregate
      type: preattestations_aggregate_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::preattestations_aggregate)
    - id: double_consensus_operation_evidence
      type: double_consensus_operation_evidence_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::double_consensus_operation_evidence)
    - id: seed_nonce_revelation
      type: seed_nonce_revelation_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::seed_nonce_revelation)
    - id: vdf_revelation
      type: solution
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::vdf_revelation)
    - id: double_baking_evidence
      type: double_baking_evidence_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::double_baking_evidence)
    - id: dal_entrapment_evidence
      type: dal_entrapment_evidence_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::dal_entrapment_evidence)
    - id: activate_account
      type: activate_account_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::activate_account)
    - id: proposals
      type: proposals_1
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::proposals)
    - id: ballot
      type: ballot
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::ballot)
    - id: reveal
      type: reveal_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::reveal)
    - id: transaction
      type: transaction_1
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::transaction)
    - id: origination
      type: origination_1
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::origination)
    - id: delegation
      type: delegation_1
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::delegation)
    - id: set_deposits_limit
      type: set_deposits_limit_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::set_deposits_limit)
    - id: increase_paid_storage
      type: increase_paid_storage_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::increase_paid_storage)
    - id: update_consensus_key
      type: update_consensus_key_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::update_consensus_key)
    - id: update_companion_key
      type: update_companion_key_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::update_companion_key)
    - id: drain_delegate
      type: drain_delegate_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::drain_delegate)
    - id: failing_noop
      type: bytes_dyn_uint30
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::failing_noop)
    - id: register_global_constant
      type: register_global_constant_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::register_global_constant)
    - id: transfer_ticket
      type: transfer_ticket_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::transfer_ticket)
    - id: dal_publish_commitment
      type: dal_publish_commitment_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::dal_publish_commitment)
    - id: smart_rollup_originate
      type: smart_rollup_originate_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_originate)
    - id: smart_rollup_add_messages
      type: smart_rollup_add_messages_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_add_messages)
    - id: smart_rollup_cement
      type: smart_rollup_cement_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_cement)
    - id: smart_rollup_publish
      type: smart_rollup_publish_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_publish)
    - id: smart_rollup_refute
      type: smart_rollup_refute_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_refute)
    - id: smart_rollup_timeout
      type: smart_rollup_timeout_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_timeout)
    - id: smart_rollup_execute_outbox_message
      type: smart_rollup_execute_outbox_message_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_execute_outbox_message)
    - id: smart_rollup_recover_bond
      type: smart_rollup_recover_bond_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::smart_rollup_recover_bond)
    - id: zk_rollup_origination
      type: zk_rollup_origination_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::zk_rollup_origination)
    - id: zk_rollup_publish
      type: zk_rollup_publish_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::zk_rollup_publish)
    - id: zk_rollup_update
      type: zk_rollup_update_0
      if: (alpha__operation__alpha__contents_tag == alpha__operation__alpha__contents_tag::zk_rollup_update)
  alpha__operation__alpha__internal_operation_result__delegation:
    seq:
    - id: alpha__operation__alpha__internal_operation_result__delegation_tag
      type: u1
      enum: alpha__operation__alpha__internal_operation_result__delegation_tag
    - id: applied
      type: applied_1
      if: (alpha__operation__alpha__internal_operation_result__delegation_tag == alpha__operation__alpha__internal_operation_result__delegation_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__internal_operation_result__delegation_tag == alpha__operation__alpha__internal_operation_result__delegation_tag::failed)
    - id: backtracked
      type: backtracked_2
      if: (alpha__operation__alpha__internal_operation_result__delegation_tag == alpha__operation__alpha__internal_operation_result__delegation_tag::backtracked)
  alpha__operation__alpha__internal_operation_result__event:
    seq:
    - id: alpha__operation__alpha__internal_operation_result__event_tag
      type: u1
      enum: alpha__operation__alpha__internal_operation_result__event_tag
    - id: applied
      type: n
      if: (alpha__operation__alpha__internal_operation_result__event_tag == alpha__operation__alpha__internal_operation_result__event_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__internal_operation_result__event_tag == alpha__operation__alpha__internal_operation_result__event_tag::failed)
    - id: backtracked
      type: backtracked
      if: (alpha__operation__alpha__internal_operation_result__event_tag == alpha__operation__alpha__internal_operation_result__event_tag::backtracked)
  alpha__operation__alpha__internal_operation_result__origination:
    seq:
    - id: alpha__operation__alpha__internal_operation_result__origination_tag
      type: u1
      enum: alpha__operation__alpha__internal_operation_result__origination_tag
    - id: applied
      type: applied_0
      if: (alpha__operation__alpha__internal_operation_result__origination_tag ==
        alpha__operation__alpha__internal_operation_result__origination_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__internal_operation_result__origination_tag ==
        alpha__operation__alpha__internal_operation_result__origination_tag::failed)
    - id: backtracked
      type: backtracked_1
      if: (alpha__operation__alpha__internal_operation_result__origination_tag ==
        alpha__operation__alpha__internal_operation_result__origination_tag::backtracked)
  alpha__operation__alpha__internal_operation_result__transaction:
    seq:
    - id: alpha__operation__alpha__internal_operation_result__transaction_tag
      type: u1
      enum: alpha__operation__alpha__internal_operation_result__transaction_tag
    - id: applied
      type: applied
      if: (alpha__operation__alpha__internal_operation_result__transaction_tag ==
        alpha__operation__alpha__internal_operation_result__transaction_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__internal_operation_result__transaction_tag ==
        alpha__operation__alpha__internal_operation_result__transaction_tag::failed)
    - id: backtracked
      type: backtracked_0
      if: (alpha__operation__alpha__internal_operation_result__transaction_tag ==
        alpha__operation__alpha__internal_operation_result__transaction_tag::backtracked)
  alpha__operation__alpha__operation_contents_and_result:
    seq:
    - id: alpha__operation__alpha__operation_contents_and_result_tag
      type: u1
      enum: alpha__operation__alpha__operation_contents_and_result_tag
    - id: attestation
      type: attestation
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::attestation)
    - id: attestation_with_dal
      type: attestation_with_dal
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::attestation_with_dal)
    - id: preattestation
      type: preattestation
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::preattestation)
    - id: attestations_aggregate
      type: attestations_aggregate
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::attestations_aggregate)
    - id: preattestations_aggregate
      type: preattestations_aggregate
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::preattestations_aggregate)
    - id: double_consensus_operation_evidence
      type: double_consensus_operation_evidence
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::double_consensus_operation_evidence)
    - id: dal_entrapment_evidence
      type: dal_entrapment_evidence
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::dal_entrapment_evidence)
    - id: seed_nonce_revelation
      type: seed_nonce_revelation
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::seed_nonce_revelation)
    - id: vdf_revelation
      type: vdf_revelation
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::vdf_revelation)
    - id: double_baking_evidence
      type: double_baking_evidence
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::double_baking_evidence)
    - id: activate_account
      type: activate_account
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::activate_account)
    - id: proposals
      type: proposals_1
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::proposals)
    - id: ballot
      type: ballot
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::ballot)
    - id: drain_delegate
      type: drain_delegate
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::drain_delegate)
    - id: reveal
      type: reveal
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::reveal)
    - id: transaction
      type: transaction_0
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::transaction)
    - id: origination
      type: origination_0
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::origination)
    - id: delegation
      type: delegation_0
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::delegation)
    - id: register_global_constant
      type: register_global_constant
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::register_global_constant)
    - id: set_deposits_limit
      type: set_deposits_limit
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::set_deposits_limit)
    - id: increase_paid_storage
      type: increase_paid_storage
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::increase_paid_storage)
    - id: update_consensus_key
      type: update_consensus_key
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::update_consensus_key)
    - id: update_companion_key
      type: update_companion_key
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::update_companion_key)
    - id: transfer_ticket
      type: transfer_ticket
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::transfer_ticket)
    - id: dal_publish_commitment
      type: dal_publish_commitment
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::dal_publish_commitment)
    - id: smart_rollup_originate
      type: smart_rollup_originate
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_originate)
    - id: smart_rollup_add_messages
      type: smart_rollup_add_messages
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_add_messages)
    - id: smart_rollup_cement
      type: smart_rollup_cement
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_cement)
    - id: smart_rollup_publish
      type: smart_rollup_publish
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_publish)
    - id: smart_rollup_refute
      type: smart_rollup_refute
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_refute)
    - id: smart_rollup_timeout
      type: smart_rollup_timeout
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_timeout)
    - id: smart_rollup_execute_outbox_message
      type: smart_rollup_execute_outbox_message
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_execute_outbox_message)
    - id: smart_rollup_recover_bond
      type: smart_rollup_recover_bond
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::smart_rollup_recover_bond)
    - id: zk_rollup_origination
      type: zk_rollup_origination
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::zk_rollup_origination)
    - id: zk_rollup_publish
      type: zk_rollup_publish
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::zk_rollup_publish)
    - id: zk_rollup_update
      type: zk_rollup_update
      if: (alpha__operation__alpha__operation_contents_and_result_tag == alpha__operation__alpha__operation_contents_and_result_tag::zk_rollup_update)
  alpha__operation__alpha__operation_result__dal_publish_commitment:
    seq:
    - id: alpha__operation__alpha__operation_result__dal_publish_commitment_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__dal_publish_commitment_tag
    - id: applied
      type: applied_7
      if: (alpha__operation__alpha__operation_result__dal_publish_commitment_tag ==
        alpha__operation__alpha__operation_result__dal_publish_commitment_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__dal_publish_commitment_tag ==
        alpha__operation__alpha__operation_result__dal_publish_commitment_tag::failed)
    - id: backtracked
      type: backtracked_8
      if: (alpha__operation__alpha__operation_result__dal_publish_commitment_tag ==
        alpha__operation__alpha__operation_result__dal_publish_commitment_tag::backtracked)
  alpha__operation__alpha__operation_result__delegation:
    seq:
    - id: alpha__operation__alpha__operation_result__delegation_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__delegation_tag
    - id: applied
      type: applied_1
      if: (alpha__operation__alpha__operation_result__delegation_tag == alpha__operation__alpha__operation_result__delegation_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__delegation_tag == alpha__operation__alpha__operation_result__delegation_tag::failed)
    - id: backtracked
      type: backtracked_2
      if: (alpha__operation__alpha__operation_result__delegation_tag == alpha__operation__alpha__operation_result__delegation_tag::backtracked)
  alpha__operation__alpha__operation_result__increase_paid_storage:
    seq:
    - id: alpha__operation__alpha__operation_result__increase_paid_storage_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__increase_paid_storage_tag
    - id: applied
      type: applied_4
      if: (alpha__operation__alpha__operation_result__increase_paid_storage_tag ==
        alpha__operation__alpha__operation_result__increase_paid_storage_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__increase_paid_storage_tag ==
        alpha__operation__alpha__operation_result__increase_paid_storage_tag::failed)
    - id: backtracked
      type: backtracked_5
      if: (alpha__operation__alpha__operation_result__increase_paid_storage_tag ==
        alpha__operation__alpha__operation_result__increase_paid_storage_tag::backtracked)
  alpha__operation__alpha__operation_result__origination:
    seq:
    - id: alpha__operation__alpha__operation_result__origination_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__origination_tag
    - id: applied
      type: applied_0
      if: (alpha__operation__alpha__operation_result__origination_tag == alpha__operation__alpha__operation_result__origination_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__origination_tag == alpha__operation__alpha__operation_result__origination_tag::failed)
    - id: backtracked
      type: backtracked_1
      if: (alpha__operation__alpha__operation_result__origination_tag == alpha__operation__alpha__operation_result__origination_tag::backtracked)
  alpha__operation__alpha__operation_result__register_global_constant:
    seq:
    - id: alpha__operation__alpha__operation_result__register_global_constant_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__register_global_constant_tag
    - id: applied
      type: applied_3
      if: (alpha__operation__alpha__operation_result__register_global_constant_tag
        == alpha__operation__alpha__operation_result__register_global_constant_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__register_global_constant_tag
        == alpha__operation__alpha__operation_result__register_global_constant_tag::failed)
    - id: backtracked
      type: backtracked_4
      if: (alpha__operation__alpha__operation_result__register_global_constant_tag
        == alpha__operation__alpha__operation_result__register_global_constant_tag::backtracked)
  alpha__operation__alpha__operation_result__reveal:
    seq:
    - id: alpha__operation__alpha__operation_result__reveal_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__reveal_tag
    - id: applied
      type: n
      if: (alpha__operation__alpha__operation_result__reveal_tag == alpha__operation__alpha__operation_result__reveal_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__reveal_tag == alpha__operation__alpha__operation_result__reveal_tag::failed)
    - id: backtracked
      type: backtracked
      if: (alpha__operation__alpha__operation_result__reveal_tag == alpha__operation__alpha__operation_result__reveal_tag::backtracked)
  alpha__operation__alpha__operation_result__set_deposits_limit:
    seq:
    - id: alpha__operation__alpha__operation_result__set_deposits_limit_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__set_deposits_limit_tag
    - id: applied
      type: n
      if: (alpha__operation__alpha__operation_result__set_deposits_limit_tag == alpha__operation__alpha__operation_result__set_deposits_limit_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__set_deposits_limit_tag == alpha__operation__alpha__operation_result__set_deposits_limit_tag::failed)
    - id: backtracked
      type: backtracked
      if: (alpha__operation__alpha__operation_result__set_deposits_limit_tag == alpha__operation__alpha__operation_result__set_deposits_limit_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_add_messages:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag
    - id: applied
      type: n
      if: (alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag
        == alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag
        == alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag::failed)
    - id: backtracked
      type: backtracked
      if: (alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag
        == alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_cement:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_cement_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_cement_tag
    - id: applied
      type: applied_9
      if: (alpha__operation__alpha__operation_result__smart_rollup_cement_tag == alpha__operation__alpha__operation_result__smart_rollup_cement_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_cement_tag == alpha__operation__alpha__operation_result__smart_rollup_cement_tag::failed)
    - id: backtracked
      type: backtracked_10
      if: (alpha__operation__alpha__operation_result__smart_rollup_cement_tag == alpha__operation__alpha__operation_result__smart_rollup_cement_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag
    - id: applied
      type: applied_12
      if: (alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag
        == alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag
        == alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag::failed)
    - id: backtracked
      type: backtracked_13
      if: (alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag
        == alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_originate:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_originate_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_originate_tag
    - id: applied
      type: applied_8
      if: (alpha__operation__alpha__operation_result__smart_rollup_originate_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_originate_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_originate_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_originate_tag::failed)
    - id: backtracked
      type: backtracked_9
      if: (alpha__operation__alpha__operation_result__smart_rollup_originate_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_originate_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_publish:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_publish_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_publish_tag
    - id: applied
      type: applied_10
      if: (alpha__operation__alpha__operation_result__smart_rollup_publish_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_publish_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_publish_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_publish_tag::failed)
    - id: backtracked
      type: backtracked_11
      if: (alpha__operation__alpha__operation_result__smart_rollup_publish_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_publish_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_recover_bond:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag
    - id: applied
      type: applied_4
      if: (alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag
        == alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag
        == alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag::failed)
    - id: backtracked
      type: backtracked_5
      if: (alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag
        == alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_refute:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_refute_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_refute_tag
    - id: applied
      type: applied_11
      if: (alpha__operation__alpha__operation_result__smart_rollup_refute_tag == alpha__operation__alpha__operation_result__smart_rollup_refute_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_refute_tag == alpha__operation__alpha__operation_result__smart_rollup_refute_tag::failed)
    - id: backtracked
      type: backtracked_12
      if: (alpha__operation__alpha__operation_result__smart_rollup_refute_tag == alpha__operation__alpha__operation_result__smart_rollup_refute_tag::backtracked)
  alpha__operation__alpha__operation_result__smart_rollup_timeout:
    seq:
    - id: alpha__operation__alpha__operation_result__smart_rollup_timeout_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__smart_rollup_timeout_tag
    - id: applied
      type: applied_11
      if: (alpha__operation__alpha__operation_result__smart_rollup_timeout_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_timeout_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__smart_rollup_timeout_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_timeout_tag::failed)
    - id: backtracked
      type: backtracked_12
      if: (alpha__operation__alpha__operation_result__smart_rollup_timeout_tag ==
        alpha__operation__alpha__operation_result__smart_rollup_timeout_tag::backtracked)
  alpha__operation__alpha__operation_result__transaction:
    seq:
    - id: alpha__operation__alpha__operation_result__transaction_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__transaction_tag
    - id: applied
      type: applied_2
      if: (alpha__operation__alpha__operation_result__transaction_tag == alpha__operation__alpha__operation_result__transaction_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__transaction_tag == alpha__operation__alpha__operation_result__transaction_tag::failed)
    - id: backtracked
      type: backtracked_3
      if: (alpha__operation__alpha__operation_result__transaction_tag == alpha__operation__alpha__operation_result__transaction_tag::backtracked)
  alpha__operation__alpha__operation_result__transfer_ticket:
    seq:
    - id: alpha__operation__alpha__operation_result__transfer_ticket_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__transfer_ticket_tag
    - id: applied
      type: applied_6
      if: (alpha__operation__alpha__operation_result__transfer_ticket_tag == alpha__operation__alpha__operation_result__transfer_ticket_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__transfer_ticket_tag == alpha__operation__alpha__operation_result__transfer_ticket_tag::failed)
    - id: backtracked
      type: backtracked_7
      if: (alpha__operation__alpha__operation_result__transfer_ticket_tag == alpha__operation__alpha__operation_result__transfer_ticket_tag::backtracked)
  alpha__operation__alpha__operation_result__update_consensus_key:
    seq:
    - id: alpha__operation__alpha__operation_result__update_consensus_key_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__update_consensus_key_tag
    - id: applied
      type: applied_5
      if: (alpha__operation__alpha__operation_result__update_consensus_key_tag ==
        alpha__operation__alpha__operation_result__update_consensus_key_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__update_consensus_key_tag ==
        alpha__operation__alpha__operation_result__update_consensus_key_tag::failed)
    - id: backtracked
      type: backtracked_6
      if: (alpha__operation__alpha__operation_result__update_consensus_key_tag ==
        alpha__operation__alpha__operation_result__update_consensus_key_tag::backtracked)
  alpha__operation__alpha__operation_result__zk_rollup_origination:
    seq:
    - id: alpha__operation__alpha__operation_result__zk_rollup_origination_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__zk_rollup_origination_tag
    - id: applied
      type: applied_13
      if: (alpha__operation__alpha__operation_result__zk_rollup_origination_tag ==
        alpha__operation__alpha__operation_result__zk_rollup_origination_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__zk_rollup_origination_tag ==
        alpha__operation__alpha__operation_result__zk_rollup_origination_tag::failed)
    - id: backtracked
      type: backtracked_14
      if: (alpha__operation__alpha__operation_result__zk_rollup_origination_tag ==
        alpha__operation__alpha__operation_result__zk_rollup_origination_tag::backtracked)
  alpha__operation__alpha__operation_result__zk_rollup_publish:
    seq:
    - id: alpha__operation__alpha__operation_result__zk_rollup_publish_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__zk_rollup_publish_tag
    - id: applied
      type: applied_14
      if: (alpha__operation__alpha__operation_result__zk_rollup_publish_tag == alpha__operation__alpha__operation_result__zk_rollup_publish_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__zk_rollup_publish_tag == alpha__operation__alpha__operation_result__zk_rollup_publish_tag::failed)
    - id: backtracked
      type: backtracked_15
      if: (alpha__operation__alpha__operation_result__zk_rollup_publish_tag == alpha__operation__alpha__operation_result__zk_rollup_publish_tag::backtracked)
  alpha__operation__alpha__operation_result__zk_rollup_update:
    seq:
    - id: alpha__operation__alpha__operation_result__zk_rollup_update_tag
      type: u1
      enum: alpha__operation__alpha__operation_result__zk_rollup_update_tag
    - id: applied
      type: applied_15
      if: (alpha__operation__alpha__operation_result__zk_rollup_update_tag == alpha__operation__alpha__operation_result__zk_rollup_update_tag::applied)
    - id: failed
      type: errors_0
      if: (alpha__operation__alpha__operation_result__zk_rollup_update_tag == alpha__operation__alpha__operation_result__zk_rollup_update_tag::failed)
    - id: backtracked
      type: backtracked_16
      if: (alpha__operation__alpha__operation_result__zk_rollup_update_tag == alpha__operation__alpha__operation_result__zk_rollup_update_tag::backtracked)
  alpha__operation__alpha__operation_with_metadata:
    seq:
    - id: alpha__operation__alpha__operation_with_metadata_tag
      type: u1
      enum: alpha__operation__alpha__operation_with_metadata_tag
    - id: operation_with_metadata
      type: operation_with_metadata
      if: (alpha__operation__alpha__operation_with_metadata_tag == alpha__operation__alpha__operation_with_metadata_tag::operation_with_metadata)
    - id: operation_without_metadata
      type: operation_without_metadata
      if: (alpha__operation__alpha__operation_with_metadata_tag == alpha__operation__alpha__operation_with_metadata_tag::operation_without_metadata)
  alpha__operation_metadata__alpha__balance_and_update:
    seq:
    - id: alpha__operation_metadata__alpha__balance_and_update_tag
      type: u1
      enum: alpha__operation_metadata__alpha__balance_and_update_tag
    - id: contract
      type: contract
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::contract)
    - id: block_fees
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::block_fees)
    - id: deposits
      type: deposits
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::deposits)
    - id: nonce_revelation_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::nonce_revelation_rewards)
    - id: attesting_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::attesting_rewards)
    - id: baking_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::baking_rewards)
    - id: baking_bonuses
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::baking_bonuses)
    - id: storage_fees
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::storage_fees)
    - id: double_signing_punishments
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::double_signing_punishments)
    - id: lost_attesting_rewards
      type: lost_attesting_rewards
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::lost_attesting_rewards)
    - id: liquidity_baking_subsidies
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::liquidity_baking_subsidies)
    - id: burned
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::burned)
    - id: commitments
      type: commitments
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::commitments)
    - id: bootstrap
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::bootstrap)
    - id: invoice
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::invoice)
    - id: initial_commitments
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::initial_commitments)
    - id: minted
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::minted)
    - id: frozen_bonds
      type: frozen_bonds
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::frozen_bonds)
    - id: smart_rollup_refutation_punishments
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::smart_rollup_refutation_punishments)
    - id: smart_rollup_refutation_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::smart_rollup_refutation_rewards)
    - id: unstaked_deposits
      type: unstaked_deposits
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::unstaked_deposits)
    - id: staking_delegator_numerator
      type: staking_delegator_numerator
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::staking_delegator_numerator)
    - id: staking_delegate_denominator
      type: staking_delegate_denominator
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::staking_delegate_denominator)
    - id: dal_attesting_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::dal_attesting_rewards)
    - id: lost_dal_attesting_rewards
      type: lost_dal_attesting_rewards
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::lost_dal_attesting_rewards)
  alpha__operation_metadata__alpha__balance_updates:
    seq:
    - id: alpha__operation_metadata__alpha__balance_updates_entries
      type: alpha__operation_metadata__alpha__balance_updates_entries
      repeat: eos
  alpha__operation_metadata__alpha__balance_updates_0:
    seq:
    - id: len_alpha__operation_metadata__alpha__balance_updates
      type: u4be
      valid:
        max: 1073741823
    - id: alpha__operation_metadata__alpha__balance_updates
      type: alpha__operation_metadata__alpha__balance_updates
      size: len_alpha__operation_metadata__alpha__balance_updates
  alpha__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: alpha__operation_metadata__alpha__balance_and_update
      type: alpha__operation_metadata__alpha__balance_and_update
    - id: alpha__operation_metadata__alpha__update_origin
      type: alpha__operation_metadata__alpha__update_origin
  alpha__operation_metadata__alpha__staking_abstract_quantity:
    seq:
    - id: change
      type: s8be
  alpha__operation_metadata__alpha__tez_balance_update:
    seq:
    - id: change
      type: s8be
  alpha__operation_metadata__alpha__update_origin:
    seq:
    - id: alpha__operation_metadata__alpha__update_origin_tag
      type: u1
      enum: alpha__operation_metadata__alpha__update_origin_tag
    - id: delayed_operation
      size: 32
      if: (alpha__operation_metadata__alpha__update_origin_tag == alpha__operation_metadata__alpha__update_origin_tag::delayed_operation)
  alpha__per_block_votes:
    seq:
    - id: alpha__per_block_votes_tag
      type: u1
      enum: alpha__per_block_votes_tag
  alpha__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  alpha__staker:
    seq:
    - id: alpha__staker_tag
      type: u1
      enum: alpha__staker_tag
    - id: single
      type: single
      if: (alpha__staker_tag == alpha__staker_tag::single)
    - id: shared
      type: public_key_hash
      if: (alpha__staker_tag == alpha__staker_tag::shared)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  alpha__transaction_destination:
    seq:
    - id: alpha__transaction_destination_tag
      type: u1
      enum: alpha__transaction_destination_tag
    - id: implicit
      type: public_key_hash
      if: (alpha__transaction_destination_tag == alpha__transaction_destination_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (alpha__transaction_destination_tag == alpha__transaction_destination_tag::originated)
    - id: smart_rollup
      type: smart_rollup
      if: (alpha__transaction_destination_tag == alpha__transaction_destination_tag::smart_rollup)
    - id: zk_rollup
      type: zk_rollup
      if: (alpha__transaction_destination_tag == alpha__transaction_destination_tag::zk_rollup)
  applied:
    seq:
    - id: applied_tag
      type: u1
      enum: applied_tag
    - id: to_contract
      type: to_contract
      if: (applied_tag == applied_tag::to_contract)
    - id: to_smart_rollup
      type: to_smart_rollup
      if: (applied_tag == applied_tag::to_smart_rollup)
  applied_0:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: originated_contracts
      type: originated_contracts_0
    - id: consumed_milligas
      type: n
    - id: storage_size
      type: z
    - id: paid_storage_size_diff
      type: z
    - id: lazy_storage_diff_tag
      type: u1
      enum: bool
    - id: lazy_storage_diff
      type: alpha__lazy_storage_diff
      if: (lazy_storage_diff_tag == bool::true)
  applied_1:
    seq:
    - id: consumed_milligas
      type: n
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
  applied_10:
    seq:
    - id: consumed_milligas
      type: n
    - id: staked_hash
      size: 32
    - id: published_at_level
      type: s4be
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
  applied_11:
    seq:
    - id: consumed_milligas
      type: n
    - id: game_status
      type: game_status
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
  applied_12:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: ticket_updates
      type: ticket_updates_0
    - id: whitelist_update_tag
      type: u1
      enum: bool
    - id: whitelist_update
      type: whitelist_update
      if: (whitelist_update_tag == bool::true)
    - id: consumed_milligas
      type: n
    - id: paid_storage_size_diff
      type: z
  applied_13:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: originated_zk_rollup
      size: 20
    - id: consumed_milligas
      type: n
    - id: size
      type: z
  applied_14:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
    - id: size
      type: z
  applied_15:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
    - id: paid_storage_size_diff
      type: z
  applied_2:
    seq:
    - id: applied_tag
      type: u1
      enum: applied_tag
    - id: to_contract
      type: to_contract_0
      if: (applied_tag == applied_tag::to_contract)
    - id: to_smart_rollup
      type: to_smart_rollup_0
      if: (applied_tag == applied_tag::to_smart_rollup)
  applied_3:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
    - id: storage_size
      type: z
    - id: global_address
      size: 32
  applied_4:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
  applied_5:
    seq:
    - id: kind
      type: u1
      enum: bool
    - id: consumed_milligas
      type: n
  applied_6:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: ticket_updates
      type: ticket_updates_0
    - id: consumed_milligas
      type: n
    - id: paid_storage_size_diff
      type: z
  applied_7:
    seq:
    - id: slot_header
      type: slot_header_0
    - id: consumed_milligas
      type: n
  applied_8:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: address
      size: 20
    - id: genesis_commitment_hash
      size: 32
    - id: consumed_milligas
      type: n
    - id: size
      type: z
  applied_9:
    seq:
    - id: consumed_milligas
      type: n
    - id: inbox_level
      type: s4be
    - id: commitment_hash
      size: 32
  args:
    seq:
    - id: args_entries
      type: args_entries
      repeat: eos
  args_0:
    seq:
    - id: len_args
      type: u4be
      valid:
        max: 1073741823
    - id: args
      type: args
      size: len_args
  args_entries:
    seq:
    - id: args_elt
      type: micheline__alpha__michelson_v1__expression
  attestation:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
    - id: metadata
      type: metadata
  attestation_0:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
  attestation_1:
    seq:
    - id: alpha__inlined__consensus_operation
      type: alpha__inlined__consensus_operation
  attestation_2:
    seq:
    - id: len_attestation
      type: u4be
      valid:
        max: 1073741823
    - id: attestation
      type: attestation_1
      size: len_attestation
  attestation_with_dal:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
    - id: dal_attestation
      type: z
    - id: metadata
      type: metadata
  attestation_with_dal_0:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
    - id: dal_attestation
      type: z
  attestations_aggregate:
    seq:
    - id: consensus_content
      type: consensus_content
    - id: committee
      type: committee_0
    - id: metadata
      type: metadata_0
  attestations_aggregate_0:
    seq:
    - id: consensus_content
      type: consensus_content
    - id: committee
      type: committee_0
  backtracked:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: consumed_milligas
      type: n
  backtracked_0:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: backtracked_tag
      type: u1
      enum: backtracked_tag
    - id: to_contract
      type: to_contract
      if: (backtracked_tag == backtracked_tag::to_contract)
    - id: to_smart_rollup
      type: to_smart_rollup
      if: (backtracked_tag == backtracked_tag::to_smart_rollup)
  backtracked_1:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: originated_contracts
      type: originated_contracts_0
    - id: consumed_milligas
      type: n
    - id: storage_size
      type: z
    - id: paid_storage_size_diff
      type: z
    - id: lazy_storage_diff_tag
      type: u1
      enum: bool
    - id: lazy_storage_diff
      type: alpha__lazy_storage_diff
      if: (lazy_storage_diff_tag == bool::true)
  backtracked_10:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: consumed_milligas
      type: n
    - id: inbox_level
      type: s4be
    - id: commitment_hash
      size: 32
  backtracked_11:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: consumed_milligas
      type: n
    - id: staked_hash
      size: 32
    - id: published_at_level
      type: s4be
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
  backtracked_12:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: consumed_milligas
      type: n
    - id: game_status
      type: game_status
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
  backtracked_13:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: ticket_updates
      type: ticket_updates_0
    - id: whitelist_update_tag
      type: u1
      enum: bool
    - id: whitelist_update
      type: whitelist_update
      if: (whitelist_update_tag == bool::true)
    - id: consumed_milligas
      type: n
    - id: paid_storage_size_diff
      type: z
  backtracked_14:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: originated_zk_rollup
      size: 20
    - id: consumed_milligas
      type: n
    - id: size
      type: z
  backtracked_15:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
    - id: size
      type: z
  backtracked_16:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
    - id: paid_storage_size_diff
      type: z
  backtracked_2:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: consumed_milligas
      type: n
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
  backtracked_3:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: backtracked_tag
      type: u1
      enum: backtracked_tag
    - id: to_contract
      type: to_contract_0
      if: (backtracked_tag == backtracked_tag::to_contract)
    - id: to_smart_rollup
      type: to_smart_rollup_0
      if: (backtracked_tag == backtracked_tag::to_smart_rollup)
  backtracked_4:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
    - id: storage_size
      type: z
    - id: global_address
      size: 32
  backtracked_5:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: consumed_milligas
      type: n
  backtracked_6:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: kind
      type: u1
      enum: bool
    - id: consumed_milligas
      type: n
  backtracked_7:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: ticket_updates
      type: ticket_updates_0
    - id: consumed_milligas
      type: n
    - id: paid_storage_size_diff
      type: z
  backtracked_8:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: slot_header
      type: slot_header_0
    - id: consumed_milligas
      type: n
  backtracked_9:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: address
      size: 20
    - id: genesis_commitment_hash
      size: 32
    - id: consumed_milligas
      type: n
    - id: size
      type: z
  baking_power:
    seq:
    - id: baking_power_tag
      type: u1
      enum: baking_power_tag
    - id: some
      type: s8be
      if: (baking_power_tag == baking_power_tag::some)
  ballot:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: period
      type: s4be
    - id: proposal
      size: 32
    - id: ballot
      type: s1
  bh1:
    seq:
    - id: alpha__block_header__alpha__full_header
      type: alpha__block_header__alpha__full_header
  bh1_0:
    seq:
    - id: len_bh1
      type: u4be
      valid:
        max: 1073741823
    - id: bh1
      type: bh1
      size: len_bh1
  bh2:
    seq:
    - id: alpha__block_header__alpha__full_header
      type: alpha__block_header__alpha__full_header
  bh2_0:
    seq:
    - id: len_bh2
      type: u4be
      valid:
        max: 1073741823
    - id: bh2
      type: bh2
      size: len_bh2
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  circuits_info:
    seq:
    - id: circuits_info_entries
      type: circuits_info_entries
      repeat: eos
  circuits_info_0:
    seq:
    - id: len_circuits_info
      type: u4be
      valid:
        max: 1073741823
    - id: circuits_info
      type: circuits_info
      size: len_circuits_info
  circuits_info_entries:
    seq:
    - id: circuits_info_elt_field0
      type: bytes_dyn_uint30
    - id: circuits_info_elt_field1
      type: u1
      enum: circuits_info_elt_field1_tag
      doc: circuits_info_elt_field1_tag
  commitment:
    seq:
    - id: compressed_state
      size: 32
    - id: inbox_level
      type: s4be
    - id: predecessor
      size: 32
    - id: number_of_ticks
      type: s8be
  commitments:
    seq:
    - id: committer
      size: 20
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  committee:
    seq:
    - id: committee_entries
      type: committee_entries
      repeat: eos
  committee_0:
    seq:
    - id: len_committee
      type: u4be
      valid:
        max: 1073741823
    - id: committee
      type: committee
      size: len_committee
  committee_1:
    seq:
    - id: committee_entries
      type: committee_entries_0
      repeat: eos
  committee_2:
    seq:
    - id: len_committee
      type: u4be
      valid:
        max: 1073741823
    - id: committee
      type: committee_1
      size: len_committee
  committee_3:
    seq:
    - id: committee_entries
      type: committee_entries_1
      repeat: eos
  committee_4:
    seq:
    - id: len_committee
      type: u4be
      valid:
        max: 1073741823
    - id: committee
      type: committee_3
      size: len_committee
  committee_entries:
    seq:
    - id: slot
      type: u2be
    - id: dal_attestation_tag
      type: u1
      enum: bool
    - id: dal_attestation
      type: z
      if: (dal_attestation_tag == bool::true)
  committee_entries_0:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: consensus_pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: consensus_power
      type: consensus_power
  committee_entries_1:
    seq:
    - id: committee_elt
      type: u2be
  consensus_content:
    seq:
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
  consensus_power:
    seq:
    - id: slots
      type: int31
    - id: baking_power
      type: baking_power
  contents:
    seq:
    - id: contents_entries
      type: contents_entries
      repeat: eos
  contents_0:
    seq:
    - id: len_contents
      type: u4be
      valid:
        max: 1073741823
    - id: contents
      type: contents
      size: len_contents
  contents_1:
    seq:
    - id: contents_entries
      type: contents_entries_0
      repeat: eos
  contents_2:
    seq:
    - id: len_contents
      type: u4be
      valid:
        max: 1073741823
    - id: contents
      type: contents_1
      size: len_contents
  contents_entries:
    seq:
    - id: alpha__operation__alpha__operation_contents_and_result
      type: alpha__operation__alpha__operation_contents_and_result
  contents_entries_0:
    seq:
    - id: alpha__operation__alpha__contents
      type: alpha__operation__alpha__contents
  contract:
    seq:
    - id: contract
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  dal__page__proof:
    seq:
    - id: dal_page_id
      type: dal_page_id
    - id: dal_proof
      type: bytes_dyn_uint30
  dal_entrapment_evidence:
    seq:
    - id: attestation
      type: attestation_2
    - id: consensus_slot
      type: u2be
    - id: slot_index
      type: u1
    - id: shard_with_proof
      type: shard_with_proof
    - id: metadata
      type: alpha__operation_metadata__alpha__balance_updates_0
  dal_entrapment_evidence_0:
    seq:
    - id: attestation
      type: attestation_2
    - id: consensus_slot
      type: u2be
    - id: slot_index
      type: u1
    - id: shard_with_proof
      type: shard_with_proof
  dal_page_id:
    seq:
    - id: published_level
      type: s4be
    - id: slot_index
      type: u1
    - id: page_index
      type: s2be
  dal_publish_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: slot_header
      type: slot_header
    - id: metadata
      type: metadata_12
  dal_publish_commitment_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: slot_header
      type: slot_header
  delegation:
    seq:
    - id: source
      type: alpha__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: nonce
      type: u2be
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: result
      type: alpha__operation__alpha__internal_operation_result__delegation
  delegation_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: metadata
      type: metadata_6
  delegation_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  deposits:
    seq:
    - id: staker
      type: alpha__frozen_staker
      doc: ! >-
        frozen_staker: Abstract notion of staker used in operation receipts for frozen
        deposits, either a single staker or all the stakers delegating to some delegate.
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  dissection:
    seq:
    - id: dissection_entries
      type: dissection_entries
      repeat: eos
  dissection_0:
    seq:
    - id: len_dissection
      type: u4be
      valid:
        max: 1073741823
    - id: dissection
      type: dissection
      size: len_dissection
  dissection_entries:
    seq:
    - id: state_tag
      type: u1
      enum: bool
    - id: state
      size: 32
      if: (state_tag == bool::true)
    - id: tick
      type: n
  double_baking_evidence:
    seq:
    - id: bh1
      type: bh1_0
    - id: bh2
      type: bh2_0
    - id: metadata
      type: metadata_1
  double_baking_evidence_0:
    seq:
    - id: bh1
      type: bh1_0
    - id: bh2
      type: bh2_0
  double_consensus_operation_evidence:
    seq:
    - id: slot
      type: u2be
    - id: op1
      type: op1_0
    - id: op2
      type: op2_0
    - id: metadata
      type: metadata_1
  double_consensus_operation_evidence_0:
    seq:
    - id: slot
      type: u2be
    - id: op1
      type: op1_0
    - id: op2
      type: op2_0
  drain_delegate:
    seq:
    - id: consensus_key
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: destination
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: metadata
      type: metadata_2
  drain_delegate_0:
    seq:
    - id: consensus_key
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: destination
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  errors:
    seq:
    - id: errors_entries
      type: errors_entries
      repeat: eos
  errors_0:
    seq:
    - id: len_errors
      type: u4be
      valid:
        max: 1073741823
    - id: errors
      type: errors
      size: len_errors
  errors_entries:
    seq:
    - id: alpha__error
      type: alpha__error
      doc: ! >-
        The full list of RPC errors would be too long to include.

        It is available at RPC `/errors` (GET).

        Errors specific to protocol Alpha have an id that starts with `proto.alpha`.
  event:
    seq:
    - id: source
      type: alpha__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: nonce
      type: u2be
    - id: type
      type: micheline__alpha__michelson_v1__expression
    - id: tag_tag
      type: u1
      enum: bool
    - id: tag
      type: alpha__entrypoint
      if: (tag_tag == bool::true)
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: payload_tag
      type: u1
      enum: bool
    - id: payload
      type: micheline__alpha__michelson_v1__expression
      if: (payload_tag == bool::true)
    - id: result
      type: alpha__operation__alpha__internal_operation_result__event
  frozen_bonds:
    seq:
    - id: contract
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: bond_id
      type: alpha__bond_id
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  game_status:
    seq:
    - id: game_status_tag
      type: u1
      enum: game_status_tag
    - id: ended
      type: result
      if: (game_status_tag == game_status_tag::ended)
  inbox__proof:
    seq:
    - id: level
      type: s4be
    - id: message_counter
      type: n
    - id: serialized_proof
      type: bytes_dyn_uint30
  increase_paid_storage:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: z
    - id: destination
      type: alpha__contract_id__originated
      doc: ! >-
        A contract handle -- originated account: A contract notation as given to an
        RPC or inside scripts. Can be a base58 originated contract hash.
    - id: metadata
      type: metadata_9
  increase_paid_storage_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: z
    - id: destination
      type: alpha__contract_id__originated
      doc: ! >-
        A contract handle -- originated account: A contract notation as given to an
        RPC or inside scripts. Can be a base58 originated contract hash.
  init_state:
    seq:
    - id: init_state_entries
      type: init_state_entries
      repeat: eos
  init_state_0:
    seq:
    - id: len_init_state
      type: u4be
      valid:
        max: 1073741823
    - id: init_state
      type: init_state
      size: len_init_state
  init_state_entries:
    seq:
    - id: init_state_elt
      size: 32
  input_proof:
    seq:
    - id: input_proof_tag
      type: u1
      enum: input_proof_tag
    - id: inbox__proof
      type: inbox__proof
      if: (input_proof_tag == input_proof_tag::inbox__proof)
    - id: reveal__proof
      type: reveal_proof
      if: (input_proof_tag == input_proof_tag::reveal__proof)
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
  internal_operation_results:
    seq:
    - id: internal_operation_results_entries
      type: internal_operation_results_entries
      repeat: eos
  internal_operation_results_0:
    seq:
    - id: len_internal_operation_results
      type: u4be
      valid:
        max: 1073741823
    - id: internal_operation_results
      type: internal_operation_results
      size: len_internal_operation_results
  internal_operation_results_entries:
    seq:
    - id: alpha__apply_internal_results__alpha__operation_result
      type: alpha__apply_internal_results__alpha__operation_result
  loser:
    seq:
    - id: reason
      type: u1
      enum: reason_tag
    - id: player
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  lost_attesting_rewards:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: participation
      type: u1
      enum: bool
    - id: revelation
      type: u1
      enum: bool
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  lost_dal_attesting_rewards:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  message:
    seq:
    - id: message_entries
      type: message_entries
      repeat: eos
  message_0:
    seq:
    - id: len_message
      type: u4be
      valid:
        max: 1073741823
    - id: message
      type: message
      size: len_message
  message_entries:
    seq:
    - id: message_elt
      type: bytes_dyn_uint30
  metadata:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: consensus_power
      type: consensus_power
    - id: consensus_key
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  metadata_0:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: committee
      type: committee_2
    - id: total_consensus_power
      type: total_consensus_power
  metadata_1:
    seq:
    - id: punished_delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: rewarded_delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: misbehaviour
      type: misbehaviour
  metadata_10:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__update_consensus_key
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_11:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__transfer_ticket
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_12:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__dal_publish_commitment
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_13:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_originate
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_14:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_add_messages
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_15:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_cement
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_16:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_publish
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_17:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_refute
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_18:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_timeout
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_19:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_2:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: allocated_destination_contract
      type: u1
      enum: bool
  metadata_20:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__smart_rollup_recover_bond
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_21:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__zk_rollup_origination
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_22:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__zk_rollup_publish
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_23:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__zk_rollup_update
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_3:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__reveal
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_4:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__transaction
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_5:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__origination
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_6:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__delegation
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_7:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__register_global_constant
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_8:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__set_deposits_limit
    - id: internal_operation_results
      type: internal_operation_results_0
  metadata_9:
    seq:
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: operation_result
      type: alpha__operation__alpha__operation_result__increase_paid_storage
    - id: internal_operation_results
      type: internal_operation_results_0
  micheline__alpha__michelson_v1__expression:
    seq:
    - id: micheline__alpha__michelson_v1__expression_tag
      type: u1
      enum: micheline__alpha__michelson_v1__expression_tag
    - id: int
      type: z
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::int)
    - id: string
      type: bytes_dyn_uint30
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::string)
    - id: sequence
      type: sequence_0
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::sequence)
    - id: prim__no_args__no_annots
      type: alpha__michelson__v1__primitives
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::prim__no_args__no_annots)
    - id: prim__no_args__some_annots
      type: prim__no_args__some_annots
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::prim__no_args__some_annots)
    - id: prim__1_arg__no_annots
      type: prim__1_arg__no_annots
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::prim__1_arg__no_annots)
    - id: prim__1_arg__some_annots
      type: prim__1_arg__some_annots
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::prim__1_arg__some_annots)
    - id: prim__2_args__no_annots
      type: prim__2_args__no_annots
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::prim__2_args__no_annots)
    - id: prim__2_args__some_annots
      type: prim__2_args__some_annots
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::prim__2_args__some_annots)
    - id: prim__generic
      type: prim__generic
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::prim__generic)
    - id: bytes
      type: bytes_dyn_uint30
      if: (micheline__alpha__michelson_v1__expression_tag == micheline__alpha__michelson_v1__expression_tag::bytes)
  misbehaviour:
    seq:
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: kind
      type: u1
      enum: kind
  move:
    seq:
    - id: choice
      type: n
    - id: step
      type: step
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
  named:
    seq:
    - id: named
      size-eos: true
  named_0:
    seq:
    - id: len_named
      type: u1
      valid:
        max: 31
    - id: named
      type: named
      size: len_named
  new_state:
    seq:
    - id: new_state_entries
      type: new_state_entries
      repeat: eos
  new_state_0:
    seq:
    - id: len_new_state
      type: u4be
      valid:
        max: 1073741823
    - id: new_state
      type: new_state
      size: len_new_state
  new_state_entries:
    seq:
    - id: new_state_elt
      size: 32
  op:
    seq:
    - id: op_entries
      type: op_entries
      repeat: eos
  op1:
    seq:
    - id: alpha__inlined__consensus_operation
      type: alpha__inlined__consensus_operation
  op1_0:
    seq:
    - id: len_op1
      type: u4be
      valid:
        max: 1073741823
    - id: op1
      type: op1
      size: len_op1
  op2:
    seq:
    - id: alpha__inlined__consensus_operation
      type: alpha__inlined__consensus_operation
  op2_0:
    seq:
    - id: len_op2
      type: u4be
      valid:
        max: 1073741823
    - id: op2
      type: op2
      size: len_op2
  op_0:
    seq:
    - id: len_op
      type: u4be
      valid:
        max: 1073741823
    - id: op
      type: op
      size: len_op
  op_elt_field0:
    seq:
    - id: op_code
      type: int31
    - id: price
      type: price
    - id: l1_dst
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: rollup_id
      size: 20
    - id: payload
      type: payload_0
  op_elt_field1:
    seq:
    - id: op_elt_field1_tag
      type: u1
      enum: op_elt_field1_tag
    - id: some
      type: some
      if: (op_elt_field1_tag == op_elt_field1_tag::some)
  op_entries:
    seq:
    - id: op_elt_field0
      type: op_elt_field0
    - id: op_elt_field1
      type: op_elt_field1
  operation_with_metadata:
    seq:
    - id: contents
      type: contents_0
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
  operation_without_metadata:
    seq:
    - id: contents
      type: contents_2
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  originated_contracts:
    seq:
    - id: originated_contracts_entries
      type: originated_contracts_entries
      repeat: eos
  originated_contracts_0:
    seq:
    - id: len_originated_contracts
      type: u4be
      valid:
        max: 1073741823
    - id: originated_contracts
      type: originated_contracts
      size: len_originated_contracts
  originated_contracts_entries:
    seq:
    - id: alpha__contract_id__originated
      type: alpha__contract_id__originated
      doc: ! >-
        A contract handle -- originated account: A contract notation as given to an
        RPC or inside scripts. Can be a base58 originated contract hash.
  origination:
    seq:
    - id: source
      type: alpha__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: nonce
      type: u2be
    - id: balance
      type: alpha__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: script
      type: alpha__scripted__contracts
    - id: result
      type: alpha__operation__alpha__internal_operation_result__origination
  origination_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: alpha__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: script
      type: alpha__scripted__contracts
    - id: metadata
      type: metadata_5
  origination_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: alpha__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: script
      type: alpha__scripted__contracts
  parameters:
    seq:
    - id: entrypoint
      type: alpha__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: bytes_dyn_uint30
  payload:
    seq:
    - id: payload_entries
      type: payload_entries
      repeat: eos
  payload_0:
    seq:
    - id: len_payload
      type: u4be
      valid:
        max: 1073741823
    - id: payload
      type: payload
      size: len_payload
  payload_entries:
    seq:
    - id: payload_elt
      size: 32
  pending_pis:
    seq:
    - id: pending_pis_entries
      type: pending_pis_entries
      repeat: eos
  pending_pis_0:
    seq:
    - id: len_pending_pis
      type: u4be
      valid:
        max: 1073741823
    - id: pending_pis
      type: pending_pis
      size: len_pending_pis
  pending_pis_elt_field1:
    seq:
    - id: new_state
      type: new_state_0
    - id: fee
      size: 32
    - id: exit_validity
      type: u1
      enum: bool
  pending_pis_entries:
    seq:
    - id: pending_pis_elt_field0
      type: bytes_dyn_uint30
    - id: pending_pis_elt_field1
      type: pending_pis_elt_field1
  preattestation:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
    - id: metadata
      type: metadata
  preattestation_0:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
  preattestations_aggregate:
    seq:
    - id: consensus_content
      type: consensus_content
    - id: committee
      type: committee_4
    - id: metadata
      type: metadata_0
  preattestations_aggregate_0:
    seq:
    - id: consensus_content
      type: consensus_content
    - id: committee
      type: committee_4
  price:
    seq:
    - id: id
      size: 32
    - id: amount
      type: z
  prim__1_arg__no_annots:
    seq:
    - id: prim
      type: alpha__michelson__v1__primitives
    - id: arg
      type: micheline__alpha__michelson_v1__expression
  prim__1_arg__some_annots:
    seq:
    - id: prim
      type: alpha__michelson__v1__primitives
    - id: arg
      type: micheline__alpha__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__2_args__no_annots:
    seq:
    - id: prim
      type: alpha__michelson__v1__primitives
    - id: arg1
      type: micheline__alpha__michelson_v1__expression
    - id: arg2
      type: micheline__alpha__michelson_v1__expression
  prim__2_args__some_annots:
    seq:
    - id: prim
      type: alpha__michelson__v1__primitives
    - id: arg1
      type: micheline__alpha__michelson_v1__expression
    - id: arg2
      type: micheline__alpha__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__generic:
    seq:
    - id: prim
      type: alpha__michelson__v1__primitives
    - id: args
      type: args_0
    - id: annots
      type: bytes_dyn_uint30
  prim__no_args__some_annots:
    seq:
    - id: prim
      type: alpha__michelson__v1__primitives
    - id: annots
      type: bytes_dyn_uint30
  private_pis:
    seq:
    - id: private_pis_entries
      type: private_pis_entries
      repeat: eos
  private_pis_0:
    seq:
    - id: len_private_pis
      type: u4be
      valid:
        max: 1073741823
    - id: private_pis
      type: private_pis
      size: len_private_pis
  private_pis_elt_field1:
    seq:
    - id: new_state
      type: new_state_0
    - id: fee
      size: 32
  private_pis_entries:
    seq:
    - id: private_pis_elt_field0
      type: bytes_dyn_uint30
    - id: private_pis_elt_field1
      type: private_pis_elt_field1
  proof:
    seq:
    - id: bls12_381_signature
      size: 96
  proof_0:
    seq:
    - id: len_proof
      type: u4be
      valid:
        max: 1073741823
    - id: proof
      type: proof
      size: len_proof
  proof_1:
    seq:
    - id: pvm_step
      type: bytes_dyn_uint30
    - id: input_proof_tag
      type: u1
      enum: bool
    - id: input_proof
      type: input_proof
      if: (input_proof_tag == bool::true)
  proposals:
    seq:
    - id: proposals_entries
      type: proposals_entries
      repeat: eos
  proposals_0:
    seq:
    - id: len_proposals
      type: u4be
      valid:
        max: 640
    - id: proposals
      type: proposals
      size: len_proposals
  proposals_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: period
      type: s4be
    - id: proposals
      type: proposals_0
  proposals_entries:
    seq:
    - id: protocol_hash
      size: 32
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
  raw_data:
    seq:
    - id: raw_data
      size-eos: true
  raw_data_0:
    seq:
    - id: len_raw_data
      type: u2be
      valid:
        max: 4096
    - id: raw_data
      type: raw_data
      size: len_raw_data
  refutation:
    seq:
    - id: refutation_tag
      type: u1
      enum: refutation_tag
    - id: start
      type: start
      if: (refutation_tag == refutation_tag::start)
    - id: move
      type: move
      if: (refutation_tag == refutation_tag::move)
  register_global_constant:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: value
      type: bytes_dyn_uint30
    - id: metadata
      type: metadata_7
  register_global_constant_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: value
      type: bytes_dyn_uint30
  result:
    seq:
    - id: result_tag
      type: u1
      enum: result_tag
    - id: loser
      type: loser
      if: (result_tag == result_tag::loser)
  reveal:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_key
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
    - id: proof_tag
      type: u1
      enum: bool
    - id: proof
      type: proof_0
      if: (proof_tag == bool::true)
    - id: metadata
      type: metadata_3
  reveal_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_key
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
    - id: proof_tag
      type: u1
      enum: bool
    - id: proof
      type: proof_0
      if: (proof_tag == bool::true)
  reveal_proof:
    seq:
    - id: reveal_proof_tag
      type: u1
      enum: reveal_proof_tag
    - id: raw__data__proof
      type: raw_data_0
      if: (reveal_proof_tag == reveal_proof_tag::raw__data__proof)
    - id: dal__page__proof
      type: dal__page__proof
      if: (reveal_proof_tag == reveal_proof_tag::dal__page__proof)
  seed_nonce_revelation:
    seq:
    - id: level
      type: s4be
    - id: nonce
      size: 32
    - id: metadata
      type: alpha__operation_metadata__alpha__balance_updates_0
  seed_nonce_revelation_0:
    seq:
    - id: level
      type: s4be
    - id: nonce
      size: 32
  sequence:
    seq:
    - id: sequence_entries
      type: sequence_entries
      repeat: eos
  sequence_0:
    seq:
    - id: len_sequence
      type: u4be
      valid:
        max: 1073741823
    - id: sequence
      type: sequence
      size: len_sequence
  sequence_entries:
    seq:
    - id: sequence_elt
      type: micheline__alpha__michelson_v1__expression
  set_deposits_limit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: limit_tag
      type: u1
      enum: bool
    - id: limit
      type: alpha__mutez
      if: (limit_tag == bool::true)
    - id: metadata
      type: metadata_8
  set_deposits_limit_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: limit_tag
      type: u1
      enum: bool
    - id: limit
      type: alpha__mutez
      if: (limit_tag == bool::true)
  shard:
    seq:
    - id: shard_field0
      type: int31
    - id: shard_field1
      type: shard_field1_0
  shard_field1:
    seq:
    - id: shard_field1_entries
      type: shard_field1_entries
      repeat: eos
  shard_field1_0:
    seq:
    - id: len_shard_field1
      type: u4be
      valid:
        max: 1073741823
    - id: shard_field1
      type: shard_field1
      size: len_shard_field1
  shard_field1_entries:
    seq:
    - id: shard_field1_elt
      size: 32
  shard_with_proof:
    seq:
    - id: shard
      type: shard
    - id: proof
      size: 48
  single:
    seq:
    - id: contract
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  slot_header:
    seq:
    - id: slot_index
      type: u1
    - id: commitment
      size: 48
    - id: commitment_proof
      size: 96
  slot_header_0:
    seq:
    - id: slot_header_tag
      type: u1
      enum: slot_header_tag
    - id: v0
      type: v0
      if: (slot_header_tag == slot_header_tag::v0)
  smart_rollup:
    seq:
    - id: smart_rollup_address
      size: 20
    - id: smart_rollup_padding
      size: 1
      doc: This field is for padding, ignore
  smart_rollup_add_messages:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: message
      type: message_0
    - id: metadata
      type: metadata_14
  smart_rollup_add_messages_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: message
      type: message_0
  smart_rollup_cement:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: metadata
      type: metadata_15
  smart_rollup_cement_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
  smart_rollup_execute_outbox_message:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: cemented_commitment
      size: 32
    - id: output_proof
      type: bytes_dyn_uint30
    - id: metadata
      type: metadata_19
  smart_rollup_execute_outbox_message_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: cemented_commitment
      size: 32
    - id: output_proof
      type: bytes_dyn_uint30
  smart_rollup_originate:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
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
    - id: metadata
      type: metadata_13
  smart_rollup_originate_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
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
  smart_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: commitment
      type: commitment
    - id: metadata
      type: metadata_16
  smart_rollup_publish_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: commitment
      type: commitment
  smart_rollup_recover_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: staker
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: metadata
      type: metadata_20
  smart_rollup_recover_bond_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: staker
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  smart_rollup_refute:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: opponent
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: refutation
      type: refutation
    - id: metadata
      type: metadata_17
  smart_rollup_refute_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: opponent
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: refutation
      type: refutation
  smart_rollup_timeout:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: stakers
      type: stakers
    - id: metadata
      type: metadata_18
  smart_rollup_timeout_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: stakers
      type: stakers
  solution:
    seq:
    - id: solution_field0
      size: 100
    - id: solution_field1
      size: 100
  some:
    seq:
    - id: contents
      type: micheline__alpha__michelson_v1__expression
    - id: ty
      type: micheline__alpha__michelson_v1__expression
    - id: ticketer
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
  stakers:
    seq:
    - id: alice
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: bob
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  staking_delegate_denominator:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: alpha__operation_metadata__alpha__staking_abstract_quantity
      type: alpha__operation_metadata__alpha__staking_abstract_quantity
  staking_delegator_numerator:
    seq:
    - id: delegator
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: alpha__operation_metadata__alpha__staking_abstract_quantity
      type: alpha__operation_metadata__alpha__staking_abstract_quantity
  start:
    seq:
    - id: player_commitment_hash
      size: 32
    - id: opponent_commitment_hash
      size: 32
  step:
    seq:
    - id: step_tag
      type: u1
      enum: step_tag
    - id: dissection
      type: dissection_0
      if: (step_tag == step_tag::dissection)
    - id: proof
      type: proof_1
      if: (step_tag == step_tag::proof)
  ticket_receipt:
    seq:
    - id: ticket_receipt_entries
      type: ticket_receipt_entries
      repeat: eos
  ticket_receipt_0:
    seq:
    - id: len_ticket_receipt
      type: u4be
      valid:
        max: 1073741823
    - id: ticket_receipt
      type: ticket_receipt
      size: len_ticket_receipt
  ticket_receipt_entries:
    seq:
    - id: ticket_token
      type: ticket_token
    - id: updates
      type: updates_0
  ticket_token:
    seq:
    - id: ticketer
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: content_type
      type: micheline__alpha__michelson_v1__expression
    - id: content
      type: micheline__alpha__michelson_v1__expression
  ticket_updates:
    seq:
    - id: ticket_updates_entries
      type: ticket_updates_entries
      repeat: eos
  ticket_updates_0:
    seq:
    - id: len_ticket_updates
      type: u4be
      valid:
        max: 1073741823
    - id: ticket_updates
      type: ticket_updates
      size: len_ticket_updates
  ticket_updates_entries:
    seq:
    - id: ticket_token
      type: ticket_token
    - id: updates
      type: updates_0
  to_contract:
    seq:
    - id: storage_tag
      type: u1
      enum: bool
    - id: storage
      type: micheline__alpha__michelson_v1__expression
      if: (storage_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: ticket_receipt
      type: ticket_receipt_0
    - id: originated_contracts
      type: originated_contracts_0
    - id: consumed_milligas
      type: n
    - id: storage_size
      type: z
    - id: paid_storage_size_diff
      type: z
    - id: allocated_destination_contract
      type: u1
      enum: bool
    - id: lazy_storage_diff_tag
      type: u1
      enum: bool
    - id: lazy_storage_diff
      type: alpha__lazy_storage_diff
      if: (lazy_storage_diff_tag == bool::true)
    - id: address_registry_diff
      type: address_registry_diff_0
  to_contract_0:
    seq:
    - id: storage_tag
      type: u1
      enum: bool
    - id: storage
      type: micheline__alpha__michelson_v1__expression
      if: (storage_tag == bool::true)
    - id: balance_updates
      type: alpha__operation_metadata__alpha__balance_updates_0
    - id: ticket_updates
      type: ticket_updates_0
    - id: originated_contracts
      type: originated_contracts_0
    - id: consumed_milligas
      type: n
    - id: storage_size
      type: z
    - id: paid_storage_size_diff
      type: z
    - id: allocated_destination_contract
      type: u1
      enum: bool
    - id: lazy_storage_diff_tag
      type: u1
      enum: bool
    - id: lazy_storage_diff
      type: alpha__lazy_storage_diff
      if: (lazy_storage_diff_tag == bool::true)
    - id: address_registry_diff
      type: address_registry_diff_0
  to_smart_rollup:
    seq:
    - id: consumed_milligas
      type: n
    - id: ticket_receipt
      type: ticket_receipt_0
  to_smart_rollup_0:
    seq:
    - id: consumed_milligas
      type: n
    - id: ticket_updates
      type: ticket_updates_0
  total_consensus_power:
    seq:
    - id: slots
      type: int31
    - id: baking_power
      type: baking_power
  transaction:
    seq:
    - id: source
      type: alpha__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: nonce
      type: u2be
    - id: amount
      type: alpha__mutez
    - id: destination
      type: alpha__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
    - id: result
      type: alpha__operation__alpha__internal_operation_result__transaction
  transaction_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: alpha__mutez
    - id: destination
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
    - id: metadata
      type: metadata_4
  transaction_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: alpha__mutez
    - id: destination
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
  transfer_ticket:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: ticket_contents
      type: bytes_dyn_uint30
    - id: ticket_ty
      type: bytes_dyn_uint30
    - id: ticket_ticketer
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: ticket_amount
      type: n
    - id: destination
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: entrypoint
      type: bytes_dyn_uint30
    - id: metadata
      type: metadata_11
  transfer_ticket_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: ticket_contents
      type: bytes_dyn_uint30
    - id: ticket_ty
      type: bytes_dyn_uint30
    - id: ticket_ticketer
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: ticket_amount
      type: n
    - id: destination
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: entrypoint
      type: bytes_dyn_uint30
  unstaked_deposits:
    seq:
    - id: staker
      type: alpha__staker
      doc: ! >-
        unstaked_frozen_staker: Abstract notion of staker used in operation receipts
        for unstaked frozen deposits, either a single staker or all the stakers delegating
        to some delegate.
    - id: cycle
      type: s4be
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  update:
    seq:
    - id: pending_pis
      type: pending_pis_0
    - id: private_pis
      type: private_pis_0
    - id: fee_pi
      type: new_state_0
    - id: proof
      type: bytes_dyn_uint30
  update_companion_key:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pk
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
    - id: proof_tag
      type: u1
      enum: bool
    - id: proof
      type: proof_0
      if: (proof_tag == bool::true)
    - id: metadata
      type: metadata_10
  update_companion_key_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pk
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
    - id: proof_tag
      type: u1
      enum: bool
    - id: proof
      type: proof_0
      if: (proof_tag == bool::true)
  update_consensus_key:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pk
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
    - id: proof_tag
      type: u1
      enum: bool
    - id: proof
      type: proof_0
      if: (proof_tag == bool::true)
    - id: metadata
      type: metadata_10
  update_consensus_key_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pk
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
    - id: proof_tag
      type: u1
      enum: bool
    - id: proof
      type: proof_0
      if: (proof_tag == bool::true)
  updates:
    seq:
    - id: updates_entries
      type: updates_entries
      repeat: eos
  updates_0:
    seq:
    - id: len_updates
      type: u4be
      valid:
        max: 1073741823
    - id: updates
      type: updates
      size: len_updates
  updates_entries:
    seq:
    - id: account
      type: alpha__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: amount
      type: z
  v0:
    seq:
    - id: level
      type: s4be
    - id: index
      type: u1
    - id: commitment
      size: 48
  vdf_revelation:
    seq:
    - id: solution
      type: solution
    - id: metadata
      type: alpha__operation_metadata__alpha__balance_updates_0
  whitelist:
    seq:
    - id: whitelist_entries
      type: whitelist_entries
      repeat: eos
  whitelist_0:
    seq:
    - id: len_whitelist
      type: u4be
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
  whitelist_update:
    seq:
    - id: whitelist_update_tag
      type: u1
      enum: whitelist_update_tag
    - id: private
      type: whitelist_0
      if: (whitelist_update_tag == whitelist_update_tag::private)
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
  zk_rollup:
    seq:
    - id: zk_rollup_hash
      size: 20
    - id: zk_rollup_padding
      size: 1
      doc: This field is for padding, ignore
  zk_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_parameters
      type: bytes_dyn_uint30
    - id: circuits_info
      type: circuits_info_0
    - id: init_state
      type: init_state_0
    - id: nb_ops
      type: int31
    - id: metadata
      type: metadata_21
  zk_rollup_origination_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_parameters
      type: bytes_dyn_uint30
    - id: circuits_info
      type: circuits_info_0
    - id: init_state
      type: init_state_0
    - id: nb_ops
      type: int31
  zk_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: zk_rollup
      size: 20
    - id: op
      type: op_0
    - id: metadata
      type: metadata_22
  zk_rollup_publish_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: zk_rollup
      size: 20
    - id: op
      type: op_0
  zk_rollup_update:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: zk_rollup
      size: 20
    - id: update
      type: update
    - id: metadata
      type: metadata_23
  zk_rollup_update_0:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: alpha__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: zk_rollup
      size: 20
    - id: update
      type: update
enums:
  alpha__apply_internal_results__alpha__operation_result_tag:
    1: transaction
    2: origination
    3: delegation
    4: event
  alpha__bond_id_tag:
    1: smart_rollup_bond_id
  alpha__contract_id__originated_tag:
    1: originated
  alpha__contract_id_tag:
    0: implicit
    1: originated
  alpha__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    5: deposit
    6: stake
    7: unstake
    8: finalize_unstake
    9: set_delegate_parameters
    255: named
  alpha__frozen_staker_tag:
    0: single
    1: shared
    2: baker
    3: baker_edge
  alpha__inlined__consensus_operation__contents_tag:
    20: preattestation
    21: attestation
    23: attestation_with_dal
    30: preattestations_aggregate
    31: attestations_aggregate
  alpha__michelson__v1__primitives:
    0: parameter
    1: storage
    2: code
    3:
      id: false
      doc: False
    4:
      id: elt
      doc: Elt
    5:
      id: left_0
      doc: Left
    6:
      id: none_0
      doc: None
    7:
      id: pair_1
      doc: Pair
    8:
      id: right
      doc: Right
    9:
      id: some_0
      doc: Some
    10:
      id: true
      doc: True
    11:
      id: unit_1
      doc: Unit
    12:
      id: pack
      doc: PACK
    13:
      id: unpack
      doc: UNPACK
    14:
      id: blake2b
      doc: BLAKE2B
    15:
      id: sha256
      doc: SHA256
    16:
      id: sha512
      doc: SHA512
    17:
      id: abs
      doc: ABS
    18:
      id: add
      doc: ADD
    19:
      id: amount
      doc: AMOUNT
    20:
      id: and
      doc: AND
    21:
      id: balance
      doc: BALANCE
    22:
      id: car
      doc: CAR
    23:
      id: cdr
      doc: CDR
    24:
      id: check_signature
      doc: CHECK_SIGNATURE
    25:
      id: compare
      doc: COMPARE
    26:
      id: concat
      doc: CONCAT
    27:
      id: cons
      doc: CONS
    28:
      id: create_account
      doc: CREATE_ACCOUNT
    29:
      id: create_contract
      doc: CREATE_CONTRACT
    30:
      id: implicit_account
      doc: IMPLICIT_ACCOUNT
    31:
      id: dip
      doc: DIP
    32:
      id: drop
      doc: DROP
    33:
      id: dup
      doc: DUP
    34:
      id: ediv
      doc: EDIV
    35:
      id: empty_map
      doc: EMPTY_MAP
    36:
      id: empty_set
      doc: EMPTY_SET
    37:
      id: eq
      doc: EQ
    38:
      id: exec
      doc: EXEC
    39:
      id: failwith
      doc: FAILWITH
    40:
      id: ge
      doc: GE
    41:
      id: get
      doc: GET
    42:
      id: gt
      doc: GT
    43:
      id: hash_key
      doc: HASH_KEY
    44:
      id: if
      doc: IF
    45:
      id: if_cons
      doc: IF_CONS
    46:
      id: if_left
      doc: IF_LEFT
    47:
      id: if_none
      doc: IF_NONE
    48:
      id: int_0
      doc: INT
    49:
      id: lambda_0
      doc: LAMBDA
    50:
      id: le
      doc: LE
    51:
      id: left
      doc: LEFT
    52:
      id: loop
      doc: LOOP
    53:
      id: lsl
      doc: LSL
    54:
      id: lsr
      doc: LSR
    55:
      id: lt
      doc: LT
    56:
      id: map_0
      doc: MAP
    57:
      id: mem
      doc: MEM
    58:
      id: mul
      doc: MUL
    59:
      id: neg
      doc: NEG
    60:
      id: neq
      doc: NEQ
    61:
      id: nil
      doc: NIL
    62:
      id: none
      doc: NONE
    63:
      id: not
      doc: NOT
    64:
      id: now
      doc: NOW
    65:
      id: or_0
      doc: OR
    66:
      id: pair_0
      doc: PAIR
    67:
      id: push
      doc: PUSH
    68:
      id: right_0
      doc: RIGHT
    69:
      id: size
      doc: SIZE
    70:
      id: some
      doc: SOME
    71:
      id: source
      doc: SOURCE
    72:
      id: sender
      doc: SENDER
    73:
      id: self
      doc: SELF
    74:
      id: steps_to_quota
      doc: STEPS_TO_QUOTA
    75:
      id: sub
      doc: SUB
    76:
      id: swap
      doc: SWAP
    77:
      id: transfer_tokens
      doc: TRANSFER_TOKENS
    78:
      id: set_delegate
      doc: SET_DELEGATE
    79:
      id: unit_0
      doc: UNIT
    80:
      id: update
      doc: UPDATE
    81:
      id: xor
      doc: XOR
    82:
      id: iter
      doc: ITER
    83:
      id: loop_left
      doc: LOOP_LEFT
    84:
      id: address_0
      doc: ADDRESS
    85:
      id: contract_0
      doc: CONTRACT
    86:
      id: isnat
      doc: ISNAT
    87:
      id: cast
      doc: CAST
    88:
      id: rename
      doc: RENAME
    89: bool
    90: contract
    91: int
    92: key
    93: key_hash
    94: lambda
    95: list
    96: map
    97: big_map
    98: nat
    99: option
    100: or
    101: pair
    102: set
    103: signature
    104: string
    105: bytes
    106: mutez
    107: timestamp
    108: unit
    109: operation
    110: address
    111:
      id: slice
      doc: SLICE
    112:
      id: dig
      doc: DIG
    113:
      id: dug
      doc: DUG
    114:
      id: empty_big_map
      doc: EMPTY_BIG_MAP
    115:
      id: apply
      doc: APPLY
    116: chain_id
    117:
      id: chain_id_0
      doc: CHAIN_ID
    118:
      id: level
      doc: LEVEL
    119:
      id: self_address
      doc: SELF_ADDRESS
    120: never
    121:
      id: never_0
      doc: NEVER
    122:
      id: unpair
      doc: UNPAIR
    123:
      id: voting_power
      doc: VOTING_POWER
    124:
      id: total_voting_power
      doc: TOTAL_VOTING_POWER
    125:
      id: keccak
      doc: KECCAK
    126:
      id: sha3
      doc: SHA3
    127:
      id: pairing_check
      doc: PAIRING_CHECK
    128: bls12_381_g1
    129: bls12_381_g2
    130: bls12_381_fr
    131: sapling_state
    132: sapling_transaction_deprecated
    133:
      id: sapling_empty_state
      doc: SAPLING_EMPTY_STATE
    134:
      id: sapling_verify_update
      doc: SAPLING_VERIFY_UPDATE
    135: ticket
    136:
      id: ticket_deprecated
      doc: TICKET_DEPRECATED
    137:
      id: read_ticket
      doc: READ_TICKET
    138:
      id: split_ticket
      doc: SPLIT_TICKET
    139:
      id: join_tickets
      doc: JOIN_TICKETS
    140:
      id: get_and_update
      doc: GET_AND_UPDATE
    141: chest
    142: chest_key
    143:
      id: open_chest
      doc: OPEN_CHEST
    144:
      id: view_0
      doc: VIEW
    145: view
    146: constant
    147:
      id: sub_mutez
      doc: SUB_MUTEZ
    148: tx_rollup_l2_address
    149:
      id: min_block_time
      doc: MIN_BLOCK_TIME
    150: sapling_transaction
    151:
      id: emit
      doc: EMIT
    152:
      id: lambda_rec_0
      doc: Lambda_rec
    153:
      id: lambda_rec
      doc: LAMBDA_REC
    154:
      id: ticket_0
      doc: TICKET
    155:
      id: bytes_0
      doc: BYTES
    156:
      id: nat_0
      doc: NAT
    157:
      id: ticket_1
      doc: Ticket
    158:
      id: is_implicit_account
      doc: IS_IMPLICIT_ACCOUNT
    159:
      id: index_address
      doc: INDEX_ADDRESS
    160:
      id: get_address_index
      doc: GET_ADDRESS_INDEX
  alpha__operation__alpha__contents_tag:
    1: seed_nonce_revelation
    2: double_consensus_operation_evidence
    3: double_baking_evidence
    4: activate_account
    5: proposals
    6: ballot
    8: vdf_revelation
    9: drain_delegate
    17: failing_noop
    20: preattestation
    21: attestation
    23: attestation_with_dal
    24: dal_entrapment_evidence
    30: preattestations_aggregate
    31: attestations_aggregate
    107: reveal
    108: transaction
    109: origination
    110: delegation
    111: register_global_constant
    112: set_deposits_limit
    113: increase_paid_storage
    114: update_consensus_key
    115: update_companion_key
    158: transfer_ticket
    200: smart_rollup_originate
    201: smart_rollup_add_messages
    202: smart_rollup_cement
    203: smart_rollup_publish
    204: smart_rollup_refute
    205: smart_rollup_timeout
    206: smart_rollup_execute_outbox_message
    207: smart_rollup_recover_bond
    230: dal_publish_commitment
    250: zk_rollup_origination
    251: zk_rollup_publish
    252: zk_rollup_update
  alpha__operation__alpha__internal_operation_result__delegation_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__internal_operation_result__event_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__internal_operation_result__origination_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__internal_operation_result__transaction_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_contents_and_result_tag:
    1: seed_nonce_revelation
    2: double_consensus_operation_evidence
    3: double_baking_evidence
    4: activate_account
    5: proposals
    6: ballot
    8: vdf_revelation
    9: drain_delegate
    20: preattestation
    21: attestation
    23: attestation_with_dal
    24: dal_entrapment_evidence
    30: preattestations_aggregate
    31: attestations_aggregate
    107: reveal
    108: transaction
    109: origination
    110: delegation
    111: register_global_constant
    112: set_deposits_limit
    113: increase_paid_storage
    114: update_consensus_key
    115: update_companion_key
    158: transfer_ticket
    200: smart_rollup_originate
    201: smart_rollup_add_messages
    202: smart_rollup_cement
    203: smart_rollup_publish
    204: smart_rollup_refute
    205: smart_rollup_timeout
    206: smart_rollup_execute_outbox_message
    207: smart_rollup_recover_bond
    230: dal_publish_commitment
    250: zk_rollup_origination
    251: zk_rollup_publish
    252: zk_rollup_update
  alpha__operation__alpha__operation_result__dal_publish_commitment_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__delegation_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__increase_paid_storage_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__origination_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__register_global_constant_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__reveal_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__set_deposits_limit_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_add_messages_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_cement_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_execute_outbox_message_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_originate_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_publish_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_recover_bond_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_refute_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__smart_rollup_timeout_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__transaction_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__transfer_ticket_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__update_consensus_key_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__zk_rollup_origination_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__zk_rollup_publish_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_result__zk_rollup_update_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  alpha__operation__alpha__operation_with_metadata_tag:
    0: operation_with_metadata
    1: operation_without_metadata
  alpha__operation_metadata__alpha__balance_and_update_tag:
    0: contract
    2: block_fees
    4: deposits
    5: nonce_revelation_rewards
    7: attesting_rewards
    8: baking_rewards
    9: baking_bonuses
    11: storage_fees
    12: double_signing_punishments
    13: lost_attesting_rewards
    14: liquidity_baking_subsidies
    15: burned
    16: commitments
    17: bootstrap
    18: invoice
    19: initial_commitments
    20: minted
    21: frozen_bonds
    24: smart_rollup_refutation_punishments
    25: smart_rollup_refutation_rewards
    26: unstaked_deposits
    27: staking_delegator_numerator
    28: staking_delegate_denominator
    29: dal_attesting_rewards
    30: lost_dal_attesting_rewards
  alpha__operation_metadata__alpha__update_origin_tag:
    0: block_application
    1: protocol_migration
    2: subsidy
    3: simulation
    4: delayed_operation
  alpha__per_block_votes_tag:
    0: per_block_vote_on
    1: per_block_vote_off
    2: per_block_vote_pass
  alpha__staker_tag:
    0: single
    1: shared
  alpha__transaction_destination_tag:
    0: implicit
    1: originated
    3: smart_rollup
    4: zk_rollup
  applied_tag:
    0: to_contract
    2: to_smart_rollup
  backtracked_tag:
    0: to_contract
    2: to_smart_rollup
  baking_power_tag:
    0: none
    1: some
  bool:
    0: false
    255: true
  circuits_info_elt_field1_tag:
    0: public
    1: private
    2: fee
  game_status_tag:
    0: ongoing
    1: ended
  input_proof_tag:
    0: inbox__proof
    1: reveal__proof
    2: first__input
  kind:
    0: preattestation
    1: attestation
    2: block
  micheline__alpha__michelson_v1__expression_tag:
    0: int
    1: string
    2: sequence
    3:
      id: prim__no_args__no_annots
      doc: Primitive with no arguments and no annotations
    4:
      id: prim__no_args__some_annots
      doc: Primitive with no arguments and some annotations
    5:
      id: prim__1_arg__no_annots
      doc: Primitive with one argument and no annotations
    6:
      id: prim__1_arg__some_annots
      doc: Primitive with one argument and some annotations
    7:
      id: prim__2_args__no_annots
      doc: Primitive with two arguments and no annotations
    8:
      id: prim__2_args__some_annots
      doc: Primitive with two arguments and some annotations
    9:
      id: prim__generic
      doc: Generic primitive (any number of args with or without annotations)
    10: bytes
  op_elt_field1_tag:
    0: none
    1: some
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
  reason_tag:
    0: conflict_resolved
    1: timeout
  refutation_tag:
    0: start
    1: move
  result_tag:
    0: loser
    1: draw
  reveal_proof_tag:
    0: raw__data__proof
    1: metadata__proof
    2: dal__page__proof
    3: dal__parameters__proof
  slot_header_tag:
    0: v0
  step_tag:
    0: dissection
    1: proof
  whitelist_update_tag:
    0: public
    1: private
seq:
- id: alpha__operation__alpha__operation_with_metadata
  type: alpha__operation__alpha__operation_with_metadata
