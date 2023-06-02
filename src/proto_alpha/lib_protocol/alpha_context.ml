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
  module Metadata = Sc_rollup_metadata_repr
  module Dissection_chunk = Sc_rollup_dissection_chunk_repr
  include Sc_rollup_PVM_sig
  module ArithPVM = Sc_rollup_arith
  module Wasm_2_0_0PVM = Sc_rollup_wasm.V2_0_0

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
    match raw_consume available_gas cost with
    | Some remaining_gas -> ok remaining_gas
    | None -> error Operation_quota_exceeded

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
    let gas_cost =
      match consume_deserialization_gas with
      | Always -> Script_repr.stable_force_decode_cost lexpr
      | When_needed -> Script_repr.force_decode_cost lexpr
    in
    Raw_context.consume_gas ctxt gas_cost >>? fun ctxt ->
    Script_repr.force_decode lexpr >|? fun v -> (v, ctxt)

  let force_bytes_in_context ctxt lexpr =
    Raw_context.consume_gas ctxt (Script_repr.force_bytes_cost lexpr)
    >>? fun ctxt ->
    Script_repr.force_bytes lexpr >|? fun v -> (v, ctxt)

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
    Raw_context.consume_gas c (Gas_limit_repr.read_bytes_cost 0) >>?= fun c ->
    Storage.Big_map.Key_type.find c id >>=? fun kt ->
    match kt with
    | None -> return (c, None)
    | Some kt ->
        Storage.Big_map.Value_type.get c id >|=? fun kv -> (c, Some (kt, kv))

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

module Receipt = Receipt_repr
module Consensus_key = Delegate_consensus_key

module Delegate = struct
  include Delegate_storage
  include Delegate_missed_endorsements_storage
  include Delegate_slashed_deposits_storage
  include Delegate_cycles

  type deposits = Deposits_repr.t = {
    initial_amount : Tez.t;
    current_amount : Tez.t;
  }

  let last_cycle_before_deactivation =
    Delegate_activation_storage.last_cycle_before_deactivation

  let prepare_stake_distribution = Stake_storage.prepare_stake_distribution

  let check_not_tz4 = Contract_delegate_storage.check_not_tz4

  let delegated_contracts = Contract_delegate_storage.delegated_contracts

  let deactivated = Delegate_activation_storage.is_inactive

  module Consensus_key = Delegate_consensus_key
  module Rewards = Delegate_rewards
end

module Stake_distribution = struct
  let snapshot = Stake_storage.snapshot

  let compute_snapshot_index = Delegate_sampler.compute_snapshot_index

  let baking_rights_owner = Delegate_sampler.baking_rights_owner

  let slot_owner = Delegate_sampler.slot_owner

  let load_sampler_for_cycle = Delegate_sampler.load_sampler_for_cycle
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

  let load_endorsement_branch ctxt =
    Storage.Tenderbake.Endorsement_branch.find ctxt >>=? function
    | Some endorsement_branch ->
        Raw_context.Consensus.set_endorsement_branch ctxt endorsement_branch
        |> return
    | None -> return ctxt

  let store_endorsement_branch ctxt branch =
    let ctxt = set_endorsement_branch ctxt branch in
    Storage.Tenderbake.Endorsement_branch.add ctxt branch
end

let prepare_first_block = Init_storage.prepare_first_block

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  Init_storage.prepare ctxt ~level ~predecessor_timestamp ~timestamp
  >>=? fun (ctxt, balance_updates, origination_results) ->
  Consensus.load_endorsement_branch ctxt >>=? fun ctxt ->
  Delegate.load_forbidden_delegates ctxt >>=? fun ctxt ->
  Adaptive_inflation_storage.load_reward_coeff ctxt >>=? fun ctxt ->
  return (ctxt, balance_updates, origination_results)

let finalize ?commit_message:message c fitness =
  let context = Raw_context.recover c in
  {
    Updater.context;
    fitness;
    message;
    max_operations_ttl = (Raw_context.constants c).max_operations_time_to_live;
    last_allowed_fork_level =
      Raw_level.to_int32 @@ Level.last_allowed_fork_level c;
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
module Toggle_EMA = Toggle_EMA
module Toggle_votes = Toggle_votes_repr

module Liquidity_baking = struct
  include Liquidity_baking_storage
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

module Internal_for_tests = struct
  let to_raw x = x
end
