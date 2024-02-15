(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type t = Raw_context.t

type context = t

module type BASIC_DATA = sig
  type t

  include Compare.S with type t := t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

module Tez = Tez_repr
module Period = Period_repr

module Timestamp = struct
  include Time_repr

  let current = Raw_context.current_timestamp

  let predecessor = Raw_context.predecessor_timestamp
end

module Slot = Slot_repr
module Sc_rollup_repr = Sc_rollup_repr

module Sc_rollup = struct
  module Tick = Sc_rollup_tick_repr
  include Sc_rollup_repr

  module Whitelist = struct
    include Sc_rollup_whitelist_storage
    include Sc_rollup_whitelist_repr
  end

  module Metadata = Sc_rollup_metadata_repr
  module Dal_parameters = Sc_rollup_dal_parameters_repr
  module Dissection_chunk = Sc_rollup_dissection_chunk_repr
  include Sc_rollup_PVM_sig
  module ArithPVM = Sc_rollup_arith
  module Wasm_2_0_0PVM = Sc_rollup_wasm.V2_0_0
  module Riscv_PVM = Sc_rollup_riscv

  module Inbox_message = struct
    include Sc_rollup_inbox_message_repr

    let protocol_migration_internal_message =
      Raw_context.protocol_migration_internal_message

    let protocol_migration_serialized_message =
      Raw_context.protocol_migration_serialized_message
  end

  module Inbox_merkelized_payload_hashes =
    Sc_rollup_inbox_merkelized_payload_hashes_repr

  module Staker = struct
    include Sc_rollup_repr.Staker
    module Index = Sc_rollup_staker_index_repr
  end

  module Inbox = struct
    include Sc_rollup_inbox_repr
    include Sc_rollup_inbox_storage

    let genesis =
      genesis
        ~protocol_migration_message:
          Inbox_message.protocol_migration_serialized_message

    let add_all_messages ~first_block =
      add_all_messages
        ~protocol_migration_message:
          (if first_block then
           Some Inbox_message.protocol_migration_internal_message
          else None)

    module Internal_for_tests = struct
      include Sc_rollup_inbox_repr.Internal_for_tests
    end
  end

  module Proof = Sc_rollup_proof_repr
  module Game = Sc_rollup_game_repr

  module Commitment = struct
    include Sc_rollup_commitment_repr
    include Sc_rollup_commitment_storage
  end

  module Stake_storage = struct
    include Sc_rollup_stake_storage
  end

  module Refutation_storage = Sc_rollup_refutation_storage
  include Sc_rollup_storage
  include Sc_rollups

  module Outbox = struct
    include Sc_rollup_outbox_storage
    module Message = Sc_rollup_outbox_message_repr
  end

  module Errors = Sc_rollup_errors
end

module Dal = struct
  include Dal_slot_repr
  include Raw_context.Dal

  module Slot_index = struct
    include Dal_slot_index_repr
  end

  module Attestation = struct
    include Dal_attestation_repr
    include Raw_context.Dal
  end

  type slot_id = Dal_slot_repr.Header.id = {
    published_level : Raw_level_repr.t;
    index : Dal_slot_index_repr.t;
  }

  module Page = struct
    include Dal_slot_repr.Page
  end

  module Slot = struct
    include Dal_slot_repr
    include Dal_slot_storage
    include Raw_context.Dal
  end

  module Operations = struct
    include Dal_operations_repr
  end

  module Slots_history = Dal_slot_repr.History
  module Slots_storage = Dal_slot_storage
end

module Dal_errors = Dal_errors_repr

module Zk_rollup = struct
  include Zk_rollup_repr
  module State = Zk_rollup_state_repr
  module Account = Zk_rollup_account_repr
  module Operation = Zk_rollup_operation_repr
  module Ticket = Zk_rollup_ticket_repr
  module Errors = Zk_rollup_errors
  module Circuit_public_inputs = Zk_rollup_circuit_public_inputs_repr
  module Update = Zk_rollup_update_repr
  include Zk_rollup_storage
end

module Entrypoint = Entrypoint_repr
module Manager_counter = Manager_counter_repr
include Operation_repr

module Operation = struct
  type 'kind t = 'kind operation = {
    shell : Operation.shell_header;
    protocol_data : 'kind protocol_data;
  }

  type packed = packed_operation

  let unsigned_encoding = unsigned_operation_encoding

  let unsigned_encoding_with_legacy_attestation_name =
    unsigned_operation_encoding_with_legacy_attestation_name

  include Operation_repr
end

module Block_header = Block_header_repr

module Vote = struct
  include Vote_repr
  include Vote_storage
end

module Block_payload = struct
  include Block_payload_repr
end

module First_level_of_protocol = struct
  let get = Storage.Tenderbake.First_level_of_protocol.get
end

module Ratio = Ratio_repr

module Raw_level = struct
  include Raw_level_repr

  module Internal_for_tests = struct
    let add = add

    let sub = sub
  end
end

module Cycle = Cycle_repr
module Fees = Fees_storage

type public_key = Signature.Public_key.t

type public_key_hash = Signature.Public_key_hash.t

type signature = Signature.t

module Constants = struct
  include Constants_repr
  include Constants_storage
  module Parametric = Constants_parametric_repr

  let round_durations ctxt = Raw_context.round_durations ctxt

  let all ctxt = all_of_parametric (parametric ctxt)
end

module Voting_period = struct
  include Voting_period_repr
  include Voting_period_storage
end

module Round = struct
  include Round_repr
  module Durations = Durations

  type round_durations = Durations.t

  let pp_round_durations = Durations.pp

  let round_durations_encoding = Durations.encoding

  let round_duration = Round_repr.Durations.round_duration

  let update ctxt round = Storage.Block_round.update ctxt round

  let get ctxt = Storage.Block_round.get ctxt
end

module Gas = struct
  include Gas_limit_repr

  type error += Block_quota_exceeded = Raw_context.Block_quota_exceeded

  type error += Operation_quota_exceeded = Raw_context.Operation_quota_exceeded

  let set_limit = Raw_context.set_gas_limit

  let consume_limit_in_block = Raw_context.consume_gas_limit_in_block

  let set_unlimited = Raw_context.set_gas_unlimited

  let consume = Raw_context.consume_gas

  let consume_from available_gas cost =
    let open Result_syntax in
    match raw_consume available_gas cost with
    | Some remaining_gas -> return remaining_gas
    | None -> tzfail Operation_quota_exceeded

  let remaining_operation_gas = Raw_context.remaining_operation_gas

  let update_remaining_operation_gas =
    Raw_context.update_remaining_operation_gas

  let reset_block_gas ctxt =
    let gas = Arith.fp @@ Constants.hard_gas_limit_per_block ctxt in
    Raw_context.update_remaining_block_gas ctxt gas

  let level = Raw_context.gas_level

  let consumed = Raw_context.gas_consumed

  let block_level = Raw_context.block_gas_level

  (* Necessary to inject costs for Storage_costs into Gas.cost *)
  let cost_of_repr cost = cost
end

module Script = struct
  include Michelson_v1_primitives
  include Script_repr

  type consume_deserialization_gas = Always | When_needed

  let force_decode_in_context ~consume_deserialization_gas ctxt lexpr =
    let open Result_syntax in
    let gas_cost =
      match consume_deserialization_gas with
      | Always -> Script_repr.stable_force_decode_cost lexpr
      | When_needed -> Script_repr.force_decode_cost lexpr
    in
    let* ctxt = Raw_context.consume_gas ctxt gas_cost in
    let+ v = Script_repr.force_decode lexpr in
    (v, ctxt)

  let force_bytes_in_context ctxt lexpr =
    let open Result_syntax in
    let* ctxt =
      Raw_context.consume_gas ctxt (Script_repr.force_bytes_cost lexpr)
    in
    let+ v = Script_repr.force_bytes lexpr in
    (v, ctxt)

  let consume_decoding_gas available_gas lexpr =
    let gas_cost = Script_repr.stable_force_decode_cost lexpr in
    Gas.consume_from available_gas gas_cost
end

module Level = struct
  include Level_repr
  include Level_storage
end

module Lazy_storage = struct
  module Kind = Lazy_storage_kind
  module IdSet = Kind.IdSet
  include Lazy_storage_diff
end

module Origination_nonce = struct
  let init = Raw_context.init_origination_nonce

  let unset = Raw_context.unset_origination_nonce

  module Internal_for_tests = Origination_nonce
end

module Destination = struct
  include Destination_repr
  include Destination_storage
end

module Contract = struct
  include Contract_repr
  include Contract_storage

  let is_manager_key_revealed = Contract_manager_storage.is_manager_key_revealed

  let check_public_key = Contract_manager_storage.check_public_key

  let reveal_manager_key = Contract_manager_storage.reveal_manager_key

  let get_manager_key = Contract_manager_storage.get_manager_key

  let is_delegate = Contract_delegate_storage.is_delegate

  type delegate_status = Contract_delegate_storage.delegate_status =
    | Delegate
    | Delegated of public_key_hash
    | Undelegated

  let get_delegate_status = Contract_delegate_storage.get_delegate_status

  module Delegate = struct
    let find = Contract_delegate_storage.find

    include Delegate_storage.Contract
  end

  module Internal_for_tests = struct
    include Contract_repr
    include Contract_storage
  end
end

module Global_constants_storage = Global_constants_storage

module Big_map = struct
  module Big_map = Lazy_storage_kind.Big_map

  module Id = struct
    type t = Big_map.Id.t

    let encoding = Big_map.Id.encoding

    let rpc_arg = Big_map.Id.rpc_arg

    let parse_z = Big_map.Id.parse_z

    let unparse_to_z = Big_map.Id.unparse_to_z
  end

  let fresh ~temporary c = Lazy_storage.fresh Big_map ~temporary c

  let mem c m k = Storage.Big_map.Contents.mem (c, m) k

  let get_opt c m k = Storage.Big_map.Contents.find (c, m) k

  let list_key_values ?offset ?length c m =
    Storage.Big_map.Contents.list_key_values ?offset ?length (c, m)

  let exists c id =
    let open Lwt_result_syntax in
    let*? c = Raw_context.consume_gas c (Gas_limit_repr.read_bytes_cost 0) in
    let* kt = Storage.Big_map.Key_type.find c id in
    match kt with
    | None -> return (c, None)
    | Some kt ->
        let+ kv = Storage.Big_map.Value_type.get c id in
        (c, Some (kt, kv))

  type update = Big_map.update = {
    key : Script_repr.expr;
    key_hash : Script_expr_hash.t;
    value : Script_repr.expr option;
  }

  type updates = Big_map.updates

  type alloc = Big_map.alloc = {
    key_type : Script_repr.expr;
    value_type : Script_repr.expr;
  }
end

module Sapling = struct
  module Sapling_state = Lazy_storage_kind.Sapling_state

  module Id = struct
    type t = Sapling_state.Id.t

    let encoding = Sapling_state.Id.encoding

    let rpc_arg = Sapling_state.Id.rpc_arg

    let parse_z = Sapling_state.Id.parse_z

    let unparse_to_z = Sapling_state.Id.unparse_to_z
  end

  include Sapling_repr
  include Sapling_storage
  include Sapling_validator

  let fresh ~temporary c = Lazy_storage.fresh Sapling_state ~temporary c

  type updates = Sapling_state.updates

  type alloc = Sapling_state.alloc = {memo_size : Sapling_repr.Memo_size.t}

  module Legacy = struct
    include Sapling.UTXO.Legacy

    let transaction_get_memo_size transaction =
      match transaction.outputs with
      | [] -> None
      | {ciphertext; _} :: _ ->
          (* Encoding ensures all ciphertexts have the same memo size. *)
          Some (Sapling.Ciphertext.get_memo_size ciphertext)

    let transaction_in_memory_size transaction =
      transaction_in_memory_size (cast transaction)

    let verify_update ctxt state transaction key =
      verify_update ctxt state (cast transaction) key
  end
end

module Bond_id = struct
  include Bond_id_repr
  module Internal_for_tests = Contract_storage
end

module Receipt = struct
  type unstaked_frozen_staker = Unstaked_frozen_staker_repr.t =
    | Single of Contract_repr.t * Signature.public_key_hash
    | Shared of Signature.public_key_hash

  type frozen_staker = Frozen_staker_repr.t = private
    | Baker of Signature.public_key_hash
    | Single_staker of {
        staker : Contract_repr.t;
        delegate : Signature.public_key_hash;
      }
    | Shared_between_stakers of {delegate : Signature.public_key_hash}

  let frozen_baker = Frozen_staker_repr.baker

  let frozen_single_staker = Frozen_staker_repr.single_staker

  let frozen_shared_between_stakers = Frozen_staker_repr.shared_between_stakers

  include Receipt_repr
end

module Consensus_key = Delegate_consensus_key
module Misbehaviour = Misbehaviour_repr

module Delegate = struct
  include Delegate_storage
  include Delegate_missed_attestations_storage
  include Delegate_slashed_deposits_storage
  include Delegate_cycles

  let last_cycle_before_deactivation =
    Delegate_activation_storage.last_cycle_before_deactivation

  let prepare_stake_distribution = Stake_storage.prepare_stake_distribution

  let check_not_tz4 = Contract_delegate_storage.check_not_tz4

  let delegated_contracts = Contract_delegate_storage.delegated_contracts

  let deactivated = Delegate_activation_storage.is_inactive

  let is_forbidden_delegate = Forbidden_delegates_storage.is_forbidden

  let already_denounced = Already_denounced_storage.already_denounced

  module Consensus_key = Delegate_consensus_key

  module Rewards = struct
    include Delegate_rewards

    module For_RPC = struct
      include Delegate_rewards.For_RPC
      include Adaptive_issuance_storage.For_RPC
    end

    module Internal_for_tests = Adaptive_issuance_storage.Internal_for_tests
  end

  module Staking_parameters = Delegate_staking_parameters
  module Shared_stake = Shared_stake

  module For_RPC = struct
    include Delegate_storage.For_RPC
    include Delegate_missed_attestations_storage.For_RPC
    include Pending_denunciations_storage.For_RPC

    let pending_denunciations = Pending_denunciations_storage.find
  end
end

module Stake_distribution = struct
  let baking_rights_owner = Delegate_sampler.baking_rights_owner

  let slot_owner = Delegate_sampler.slot_owner

  let load_sampler_for_cycle = Delegate_sampler.load_sampler_for_cycle

  let get_total_frozen_stake ctxt cycle =
    let open Lwt_result_syntax in
    let* total_stake = Stake_storage.get_total_active_stake ctxt cycle in
    return (Stake_repr.get_frozen total_stake)

  module For_RPC = Delegate_sampler.For_RPC

  module Internal_for_tests = struct
    let get_selected_distribution = Stake_storage.get_selected_distribution
  end
end

module Staking = struct
  include Staking

  let stake = stake ~for_next_cycle_use_only_after_slashing:false

  let request_unstake =
    request_unstake ~for_next_cycle_use_only_after_slashing:false

  let finalize_unstake =
    finalize_unstake ~for_next_cycle_use_only_after_slashing:false
end

module Nonce = Nonce_storage

module Seed = struct
  include Seed_repr
  include Seed_storage
end

module Fitness = struct
  type raw = Fitness.t

  include Fitness_repr
end

module Bootstrap = Bootstrap_storage

module Commitment = struct
  include Commitment_repr
  include Commitment_storage
end

module Migration = Migration_repr

module Consensus = struct
  include Raw_context.Consensus

  let load_attestation_branch ctxt =
    let open Lwt_result_syntax in
    let* result = Storage.Tenderbake.Attestation_branch.find ctxt in
    match result with
    | Some attestation_branch ->
        Raw_context.Consensus.set_attestation_branch ctxt attestation_branch
        |> return
    | None -> return ctxt

  let store_attestation_branch ctxt branch =
    let ctxt = set_attestation_branch ctxt branch in
    Storage.Tenderbake.Attestation_branch.add ctxt branch
end

let prepare_first_block = Init_storage.prepare_first_block

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  let open Lwt_result_syntax in
  let* ctxt, balance_updates, origination_results =
    Init_storage.prepare ctxt ~level ~predecessor_timestamp ~timestamp
  in
  let* ctxt = Consensus.load_attestation_branch ctxt in
  let* ctxt = Forbidden_delegates_storage.load ctxt in
  let* ctxt = Adaptive_issuance_storage.load_reward_coeff ctxt in
  return (ctxt, balance_updates, origination_results)

let finalize ?commit_message:message c fitness =
  let context = Raw_context.recover c in
  {
    Updater.context;
    fitness;
    message;
    max_operations_ttl = (Raw_context.constants c).max_operations_time_to_live;
    last_finalized_block_level =
      Raw_level.to_int32 (Level.last_finalized_block_level c);
    last_preserved_block_level =
      Raw_level.to_int32 (Level.last_preserved_block_level c);
  }

let current_context c = Raw_context.recover c

let record_non_consensus_operation_hash =
  Raw_context.record_non_consensus_operation_hash

let non_consensus_operations = Raw_context.non_consensus_operations

let record_dictator_proposal_seen = Raw_context.record_dictator_proposal_seen

let dictator_proposal_seen = Raw_context.dictator_proposal_seen

let activate = Raw_context.activate

let reset_internal_nonce = Raw_context.reset_internal_nonce

let fresh_internal_nonce = Raw_context.fresh_internal_nonce

let record_internal_nonce = Raw_context.record_internal_nonce

let internal_nonce_already_recorded =
  Raw_context.internal_nonce_already_recorded

let description = Raw_context.description

module Parameters = Parameters_repr
module Votes_EMA = Votes_EMA_repr
module Per_block_votes = Per_block_votes_repr

module Liquidity_baking = struct
  include Liquidity_baking_storage
end

module Adaptive_issuance = struct
  include Adaptive_issuance_storage
end

module Ticket_hash = struct
  include Ticket_hash_repr
  include Ticket_hash_builder
end

module Ticket_balance = struct
  include Ticket_storage
end

module Token = Token
module Cache = Cache_repr

module Unstake_requests = struct
  include Unstake_requests_storage

  let prepare_finalize_unstake =
    prepare_finalize_unstake ~for_next_cycle_use_only_after_slashing:false

  module For_RPC = struct
    let apply_slash_to_unstaked_unfinalizable ctxt ~delegate ~requests =
      Unstake_requests_storage.For_RPC.apply_slash_to_unstaked_unfinalizable
        ctxt
        {delegate; requests}

    let apply_slash_to_unstaked_unfinalizable_stored_requests ctxt
        {delegate; requests} =
      let open Lwt_result_syntax in
      let* requests =
        Unstake_requests_storage.For_RPC.apply_slash_to_unstaked_unfinalizable
          ctxt
          {delegate; requests}
      in
      return {delegate; requests}
  end
end

module Unstaked_frozen_deposits = Unstaked_frozen_deposits_storage

module Staking_pseudotoken = struct
  include Staking_pseudotoken_repr
  module For_RPC = Staking_pseudotoken_repr
  module Internal_for_tests = Staking_pseudotoken_repr
end

module Staking_pseudotokens = struct
  include Staking_pseudotokens_storage
  module For_RPC = Staking_pseudotokens_storage.For_RPC
end

module Internal_for_tests = struct
  let to_raw x = x
end
