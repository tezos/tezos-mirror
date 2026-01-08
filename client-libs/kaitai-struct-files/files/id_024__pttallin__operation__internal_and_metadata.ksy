meta:
  id: id_024__pttallin__operation__internal_and_metadata
  endian: be
  imports:
  - id_024__pttallin__lazy_storage_diff
doc: ! 'Encoding id: 024-PtTALLiN.operation.internal_and_metadata'
types:
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
      type: id_024__pttallin__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: index
      type: z
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
      type: id_024__pttallin__operation_metadata__alpha__balance_updates_0
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
      type: id_024__pttallin__lazy_storage_diff
      if: (lazy_storage_diff_tag == bool::true)
  applied_1:
    seq:
    - id: consumed_milligas
      type: n
    - id: balance_updates
      type: id_024__pttallin__operation_metadata__alpha__balance_updates_0
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
      type: micheline__024__pttallin__michelson_v1__expression
  backtracked:
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
  backtracked_0:
    seq:
    - id: errors_tag
      type: u1
      enum: bool
    - id: errors
      type: errors_0
      if: (errors_tag == bool::true)
    - id: balance_updates
      type: id_024__pttallin__operation_metadata__alpha__balance_updates_0
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
      type: id_024__pttallin__lazy_storage_diff
      if: (lazy_storage_diff_tag == bool::true)
  backtracked_1:
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
      type: id_024__pttallin__operation_metadata__alpha__balance_updates_0
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
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  commitments:
    seq:
    - id: committer
      size: 20
    - id: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
  contract:
    seq:
    - id: contract
      type: id_024__pttallin__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
  delegation:
    seq:
    - id: source
      type: id_024__pttallin__transaction_destination
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
      type: id_024__pttallin__operation__alpha__internal_operation_result__delegation
  deposits:
    seq:
    - id: staker
      type: id_024__pttallin__frozen_staker
      doc: ! >-
        frozen_staker: Abstract notion of staker used in operation receipts for frozen
        deposits, either a single staker or all the stakers delegating to some delegate.
    - id: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
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
    - id: id_024__pttallin__error
      type: id_024__pttallin__error
      doc: ! >-
        The full list of RPC errors would be too long to include.

        It is available at RPC `/errors` (GET).

        Errors specific to protocol Alpha have an id that starts with `proto.alpha`.
  event:
    seq:
    - id: source
      type: id_024__pttallin__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: nonce
      type: u2be
    - id: type
      type: micheline__024__pttallin__michelson_v1__expression
    - id: tag_tag
      type: u1
      enum: bool
    - id: tag
      type: id_024__pttallin__entrypoint
      if: (tag_tag == bool::true)
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: payload_tag
      type: u1
      enum: bool
    - id: payload
      type: micheline__024__pttallin__michelson_v1__expression
      if: (payload_tag == bool::true)
    - id: result
      type: id_024__pttallin__operation__alpha__internal_operation_result__event
  frozen_bonds:
    seq:
    - id: contract
      type: id_024__pttallin__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: bond_id
      type: id_024__pttallin__bond_id
    - id: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
  id_024__pttallin__apply_internal_results__alpha__operation_result:
    seq:
    - id: id_024__pttallin__apply_internal_results__alpha__operation_result_tag
      type: u1
      enum: id_024__pttallin__apply_internal_results__alpha__operation_result_tag
    - id: transaction
      type: transaction
      if: (id_024__pttallin__apply_internal_results__alpha__operation_result_tag ==
        id_024__pttallin__apply_internal_results__alpha__operation_result_tag::transaction)
    - id: origination
      type: origination
      if: (id_024__pttallin__apply_internal_results__alpha__operation_result_tag ==
        id_024__pttallin__apply_internal_results__alpha__operation_result_tag::origination)
    - id: delegation
      type: delegation
      if: (id_024__pttallin__apply_internal_results__alpha__operation_result_tag ==
        id_024__pttallin__apply_internal_results__alpha__operation_result_tag::delegation)
    - id: event
      type: event
      if: (id_024__pttallin__apply_internal_results__alpha__operation_result_tag ==
        id_024__pttallin__apply_internal_results__alpha__operation_result_tag::event)
  id_024__pttallin__bond_id:
    seq:
    - id: id_024__pttallin__bond_id_tag
      type: u1
      enum: id_024__pttallin__bond_id_tag
    - id: smart_rollup_bond_id
      size: 20
      if: (id_024__pttallin__bond_id_tag == id_024__pttallin__bond_id_tag::smart_rollup_bond_id)
  id_024__pttallin__contract_id:
    seq:
    - id: id_024__pttallin__contract_id_tag
      type: u1
      enum: id_024__pttallin__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_024__pttallin__contract_id_tag == id_024__pttallin__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (id_024__pttallin__contract_id_tag == id_024__pttallin__contract_id_tag::originated)
  id_024__pttallin__contract_id__originated:
    seq:
    - id: id_024__pttallin__contract_id__originated_tag
      type: u1
      enum: id_024__pttallin__contract_id__originated_tag
    - id: originated
      type: originated
      if: (id_024__pttallin__contract_id__originated_tag == id_024__pttallin__contract_id__originated_tag::originated)
  id_024__pttallin__entrypoint:
    seq:
    - id: id_024__pttallin__entrypoint_tag
      type: u1
      enum: id_024__pttallin__entrypoint_tag
    - id: named
      type: named_0
      if: (id_024__pttallin__entrypoint_tag == id_024__pttallin__entrypoint_tag::named)
  id_024__pttallin__error:
    seq:
    - id: id_024__pttallin__error
      type: bytes_dyn_uint30
  id_024__pttallin__frozen_staker:
    seq:
    - id: id_024__pttallin__frozen_staker_tag
      type: u1
      enum: id_024__pttallin__frozen_staker_tag
    - id: single
      type: single
      if: (id_024__pttallin__frozen_staker_tag == id_024__pttallin__frozen_staker_tag::single)
    - id: shared
      type: public_key_hash
      if: (id_024__pttallin__frozen_staker_tag == id_024__pttallin__frozen_staker_tag::shared)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: baker
      type: public_key_hash
      if: (id_024__pttallin__frozen_staker_tag == id_024__pttallin__frozen_staker_tag::baker)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: baker_edge
      type: public_key_hash
      if: (id_024__pttallin__frozen_staker_tag == id_024__pttallin__frozen_staker_tag::baker_edge)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  id_024__pttallin__michelson__v1__primitives:
    seq:
    - id: id_024__pttallin__michelson__v1__primitives
      type: u1
      enum: id_024__pttallin__michelson__v1__primitives
  id_024__pttallin__mutez:
    seq:
    - id: id_024__pttallin__mutez
      type: n
  id_024__pttallin__operation__alpha__internal_operation_result__delegation:
    seq:
    - id: id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag
      type: u1
      enum: id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag
    - id: applied
      type: applied_1
      if: (id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag::applied)
    - id: failed
      type: errors_0
      if: (id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag::failed)
    - id: backtracked
      type: backtracked_1
      if: (id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag::backtracked)
  id_024__pttallin__operation__alpha__internal_operation_result__event:
    seq:
    - id: id_024__pttallin__operation__alpha__internal_operation_result__event_tag
      type: u1
      enum: id_024__pttallin__operation__alpha__internal_operation_result__event_tag
    - id: applied
      type: n
      if: (id_024__pttallin__operation__alpha__internal_operation_result__event_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__event_tag::applied)
    - id: failed
      type: errors_0
      if: (id_024__pttallin__operation__alpha__internal_operation_result__event_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__event_tag::failed)
    - id: backtracked
      type: backtracked_2
      if: (id_024__pttallin__operation__alpha__internal_operation_result__event_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__event_tag::backtracked)
  id_024__pttallin__operation__alpha__internal_operation_result__origination:
    seq:
    - id: id_024__pttallin__operation__alpha__internal_operation_result__origination_tag
      type: u1
      enum: id_024__pttallin__operation__alpha__internal_operation_result__origination_tag
    - id: applied
      type: applied_0
      if: (id_024__pttallin__operation__alpha__internal_operation_result__origination_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__origination_tag::applied)
    - id: failed
      type: errors_0
      if: (id_024__pttallin__operation__alpha__internal_operation_result__origination_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__origination_tag::failed)
    - id: backtracked
      type: backtracked_0
      if: (id_024__pttallin__operation__alpha__internal_operation_result__origination_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__origination_tag::backtracked)
  id_024__pttallin__operation__alpha__internal_operation_result__transaction:
    seq:
    - id: id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag
      type: u1
      enum: id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag
    - id: applied
      type: applied
      if: (id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag::applied)
    - id: failed
      type: errors_0
      if: (id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag::failed)
    - id: backtracked
      type: backtracked
      if: (id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag
        == id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag::backtracked)
  id_024__pttallin__operation_metadata__alpha__balance_and_update:
    seq:
    - id: id_024__pttallin__operation_metadata__alpha__balance_and_update_tag
      type: u1
      enum: id_024__pttallin__operation_metadata__alpha__balance_and_update_tag
    - id: contract
      type: contract
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::contract)
    - id: block_fees
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::block_fees)
    - id: deposits
      type: deposits
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::deposits)
    - id: nonce_revelation_rewards
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::nonce_revelation_rewards)
    - id: attesting_rewards
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::attesting_rewards)
    - id: baking_rewards
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::baking_rewards)
    - id: baking_bonuses
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::baking_bonuses)
    - id: storage_fees
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::storage_fees)
    - id: double_signing_punishments
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::double_signing_punishments)
    - id: lost_attesting_rewards
      type: lost_attesting_rewards
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::lost_attesting_rewards)
    - id: liquidity_baking_subsidies
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::liquidity_baking_subsidies)
    - id: burned
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::burned)
    - id: commitments
      type: commitments
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::commitments)
    - id: bootstrap
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::bootstrap)
    - id: invoice
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::invoice)
    - id: initial_commitments
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::initial_commitments)
    - id: minted
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::minted)
    - id: frozen_bonds
      type: frozen_bonds
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::frozen_bonds)
    - id: smart_rollup_refutation_punishments
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::smart_rollup_refutation_punishments)
    - id: smart_rollup_refutation_rewards
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::smart_rollup_refutation_rewards)
    - id: unstaked_deposits
      type: unstaked_deposits
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::unstaked_deposits)
    - id: staking_delegator_numerator
      type: staking_delegator_numerator
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::staking_delegator_numerator)
    - id: staking_delegate_denominator
      type: staking_delegate_denominator
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::staking_delegate_denominator)
    - id: dal_attesting_rewards
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::dal_attesting_rewards)
    - id: lost_dal_attesting_rewards
      type: lost_dal_attesting_rewards
      if: (id_024__pttallin__operation_metadata__alpha__balance_and_update_tag ==
        id_024__pttallin__operation_metadata__alpha__balance_and_update_tag::lost_dal_attesting_rewards)
  id_024__pttallin__operation_metadata__alpha__balance_updates:
    seq:
    - id: id_024__pttallin__operation_metadata__alpha__balance_updates_entries
      type: id_024__pttallin__operation_metadata__alpha__balance_updates_entries
      repeat: eos
  id_024__pttallin__operation_metadata__alpha__balance_updates_0:
    seq:
    - id: len_id_024__pttallin__operation_metadata__alpha__balance_updates
      type: u4be
      valid:
        max: 1073741823
    - id: id_024__pttallin__operation_metadata__alpha__balance_updates
      type: id_024__pttallin__operation_metadata__alpha__balance_updates
      size: len_id_024__pttallin__operation_metadata__alpha__balance_updates
  id_024__pttallin__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: id_024__pttallin__operation_metadata__alpha__balance_and_update
      type: id_024__pttallin__operation_metadata__alpha__balance_and_update
    - id: id_024__pttallin__operation_metadata__alpha__update_origin
      type: id_024__pttallin__operation_metadata__alpha__update_origin
  id_024__pttallin__operation_metadata__alpha__staking_abstract_quantity:
    seq:
    - id: change
      type: s8be
  id_024__pttallin__operation_metadata__alpha__tez_balance_update:
    seq:
    - id: change
      type: s8be
  id_024__pttallin__operation_metadata__alpha__update_origin:
    seq:
    - id: id_024__pttallin__operation_metadata__alpha__update_origin_tag
      type: u1
      enum: id_024__pttallin__operation_metadata__alpha__update_origin_tag
    - id: delayed_operation
      size: 32
      if: (id_024__pttallin__operation_metadata__alpha__update_origin_tag == id_024__pttallin__operation_metadata__alpha__update_origin_tag::delayed_operation)
  id_024__pttallin__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  id_024__pttallin__staker:
    seq:
    - id: id_024__pttallin__staker_tag
      type: u1
      enum: id_024__pttallin__staker_tag
    - id: single
      type: single
      if: (id_024__pttallin__staker_tag == id_024__pttallin__staker_tag::single)
    - id: shared
      type: public_key_hash
      if: (id_024__pttallin__staker_tag == id_024__pttallin__staker_tag::shared)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  id_024__pttallin__transaction_destination:
    seq:
    - id: id_024__pttallin__transaction_destination_tag
      type: u1
      enum: id_024__pttallin__transaction_destination_tag
    - id: implicit
      type: public_key_hash
      if: (id_024__pttallin__transaction_destination_tag == id_024__pttallin__transaction_destination_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (id_024__pttallin__transaction_destination_tag == id_024__pttallin__transaction_destination_tag::originated)
    - id: smart_rollup
      type: smart_rollup
      if: (id_024__pttallin__transaction_destination_tag == id_024__pttallin__transaction_destination_tag::smart_rollup)
    - id: zk_rollup
      type: zk_rollup
      if: (id_024__pttallin__transaction_destination_tag == id_024__pttallin__transaction_destination_tag::zk_rollup)
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
    - id: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
  lost_dal_attesting_rewards:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
  micheline__024__pttallin__michelson_v1__expression:
    seq:
    - id: micheline__024__pttallin__michelson_v1__expression_tag
      type: u1
      enum: micheline__024__pttallin__michelson_v1__expression_tag
    - id: int
      type: z
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::int)
    - id: string
      type: bytes_dyn_uint30
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::string)
    - id: sequence
      type: sequence_0
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::sequence)
    - id: prim__no_args__no_annots
      type: id_024__pttallin__michelson__v1__primitives
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::prim__no_args__no_annots)
    - id: prim__no_args__some_annots
      type: prim__no_args__some_annots
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::prim__no_args__some_annots)
    - id: prim__1_arg__no_annots
      type: prim__1_arg__no_annots
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::prim__1_arg__no_annots)
    - id: prim__1_arg__some_annots
      type: prim__1_arg__some_annots
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::prim__1_arg__some_annots)
    - id: prim__2_args__no_annots
      type: prim__2_args__no_annots
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::prim__2_args__no_annots)
    - id: prim__2_args__some_annots
      type: prim__2_args__some_annots
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::prim__2_args__some_annots)
    - id: prim__generic
      type: prim__generic
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::prim__generic)
    - id: bytes
      type: bytes_dyn_uint30
      if: (micheline__024__pttallin__michelson_v1__expression_tag == micheline__024__pttallin__michelson_v1__expression_tag::bytes)
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
    - id: id_024__pttallin__contract_id__originated
      type: id_024__pttallin__contract_id__originated
      doc: ! >-
        A contract handle -- originated account: A contract notation as given to an
        RPC or inside scripts. Can be a base58 originated contract hash.
  origination:
    seq:
    - id: source
      type: id_024__pttallin__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: nonce
      type: u2be
    - id: balance
      type: id_024__pttallin__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: script
      type: id_024__pttallin__scripted__contracts
    - id: result
      type: id_024__pttallin__operation__alpha__internal_operation_result__origination
  parameters:
    seq:
    - id: entrypoint
      type: id_024__pttallin__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: bytes_dyn_uint30
  prim__1_arg__no_annots:
    seq:
    - id: prim
      type: id_024__pttallin__michelson__v1__primitives
    - id: arg
      type: micheline__024__pttallin__michelson_v1__expression
  prim__1_arg__some_annots:
    seq:
    - id: prim
      type: id_024__pttallin__michelson__v1__primitives
    - id: arg
      type: micheline__024__pttallin__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__2_args__no_annots:
    seq:
    - id: prim
      type: id_024__pttallin__michelson__v1__primitives
    - id: arg1
      type: micheline__024__pttallin__michelson_v1__expression
    - id: arg2
      type: micheline__024__pttallin__michelson_v1__expression
  prim__2_args__some_annots:
    seq:
    - id: prim
      type: id_024__pttallin__michelson__v1__primitives
    - id: arg1
      type: micheline__024__pttallin__michelson_v1__expression
    - id: arg2
      type: micheline__024__pttallin__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__generic:
    seq:
    - id: prim
      type: id_024__pttallin__michelson__v1__primitives
    - id: args
      type: args_0
    - id: annots
      type: bytes_dyn_uint30
  prim__no_args__some_annots:
    seq:
    - id: prim
      type: id_024__pttallin__michelson__v1__primitives
    - id: annots
      type: bytes_dyn_uint30
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
      type: micheline__024__pttallin__michelson_v1__expression
  single:
    seq:
    - id: contract
      type: id_024__pttallin__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  smart_rollup:
    seq:
    - id: smart_rollup_address
      size: 20
    - id: smart_rollup_padding
      size: 1
      doc: This field is for padding, ignore
  staking_delegate_denominator:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: id_024__pttallin__operation_metadata__alpha__staking_abstract_quantity
      type: id_024__pttallin__operation_metadata__alpha__staking_abstract_quantity
  staking_delegator_numerator:
    seq:
    - id: delegator
      type: id_024__pttallin__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: id_024__pttallin__operation_metadata__alpha__staking_abstract_quantity
      type: id_024__pttallin__operation_metadata__alpha__staking_abstract_quantity
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
      type: id_024__pttallin__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: content_type
      type: micheline__024__pttallin__michelson_v1__expression
    - id: content
      type: micheline__024__pttallin__michelson_v1__expression
  to_contract:
    seq:
    - id: storage_tag
      type: u1
      enum: bool
    - id: storage
      type: micheline__024__pttallin__michelson_v1__expression
      if: (storage_tag == bool::true)
    - id: balance_updates
      type: id_024__pttallin__operation_metadata__alpha__balance_updates_0
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
      type: id_024__pttallin__lazy_storage_diff
      if: (lazy_storage_diff_tag == bool::true)
    - id: address_registry_diff
      type: address_registry_diff_0
  to_smart_rollup:
    seq:
    - id: consumed_milligas
      type: n
    - id: ticket_receipt
      type: ticket_receipt_0
  transaction:
    seq:
    - id: source
      type: id_024__pttallin__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: nonce
      type: u2be
    - id: amount
      type: id_024__pttallin__mutez
    - id: destination
      type: id_024__pttallin__transaction_destination
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
      type: id_024__pttallin__operation__alpha__internal_operation_result__transaction
  unstaked_deposits:
    seq:
    - id: staker
      type: id_024__pttallin__staker
      doc: ! >-
        unstaked_frozen_staker: Abstract notion of staker used in operation receipts
        for unstaked frozen deposits, either a single staker or all the stakers delegating
        to some delegate.
    - id: cycle
      type: s4be
    - id: id_024__pttallin__operation_metadata__alpha__tez_balance_update
      type: id_024__pttallin__operation_metadata__alpha__tez_balance_update
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
      type: id_024__pttallin__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, a base58 originated transaction
        rollup, or a base58 originated smart rollup.
    - id: amount
      type: z
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
enums:
  applied_tag:
    0: to_contract
    2: to_smart_rollup
  backtracked_tag:
    0: to_contract
    2: to_smart_rollup
  bool:
    0: false
    255: true
  id_024__pttallin__apply_internal_results__alpha__operation_result_tag:
    1: transaction
    2: origination
    3: delegation
    4: event
  id_024__pttallin__bond_id_tag:
    1: smart_rollup_bond_id
  id_024__pttallin__contract_id__originated_tag:
    1: originated
  id_024__pttallin__contract_id_tag:
    0: implicit
    1: originated
  id_024__pttallin__entrypoint_tag:
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
  id_024__pttallin__frozen_staker_tag:
    0: single
    1: shared
    2: baker
    3: baker_edge
  id_024__pttallin__michelson__v1__primitives:
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
  id_024__pttallin__operation__alpha__internal_operation_result__delegation_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  id_024__pttallin__operation__alpha__internal_operation_result__event_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  id_024__pttallin__operation__alpha__internal_operation_result__origination_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  id_024__pttallin__operation__alpha__internal_operation_result__transaction_tag:
    0: applied
    1: failed
    2: skipped
    3: backtracked
  id_024__pttallin__operation_metadata__alpha__balance_and_update_tag:
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
  id_024__pttallin__operation_metadata__alpha__update_origin_tag:
    0: block_application
    1: protocol_migration
    2: subsidy
    3: simulation
    4: delayed_operation
  id_024__pttallin__staker_tag:
    0: single
    1: shared
  id_024__pttallin__transaction_destination_tag:
    0: implicit
    1: originated
    3: smart_rollup
    4: zk_rollup
  micheline__024__pttallin__michelson_v1__expression_tag:
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
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
seq:
- id: id_024__pttallin__apply_internal_results__alpha__operation_result
  type: id_024__pttallin__apply_internal_results__alpha__operation_result
