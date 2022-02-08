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

module Sc_rollup = struct
  module Tick = Sc_rollup_tick_repr
  include Sc_rollup_repr
  module Inbox = Sc_rollup_inbox_repr
  include Sc_rollup_storage
end

module Entrypoint = Entrypoint_repr
include Operation_repr

module Operation = struct
  type 'kind t = 'kind operation = {
    shell : Operation.shell_header;
    protocol_data : 'kind protocol_data;
  }

  type packed = packed_operation

  let unsigned_encoding = unsigned_operation_encoding

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

module Raw_level = Raw_level_repr
module Cycle = Cycle_repr
module Script_string = Script_string_repr
module Script_int = Script_int_repr

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
end

module Fees = Fees_storage

type public_key = Signature.Public_key.t

type public_key_hash = Signature.Public_key_hash.t

type signature = Signature.t

module Constants = struct
  include Constants_repr
  include Constants_storage

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

  type error += Gas_limit_too_high = Raw_context.Gas_limit_too_high

  type error += Block_quota_exceeded = Raw_context.Block_quota_exceeded

  type error += Operation_quota_exceeded = Raw_context.Operation_quota_exceeded

  let check_limit_is_valid = Raw_context.check_gas_limit_is_valid

  let set_limit = Raw_context.set_gas_limit

  let consume_limit_in_block = Raw_context.consume_gas_limit_in_block

  let set_unlimited = Raw_context.set_gas_unlimited

  let consume = Raw_context.consume_gas

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

module Destination = Destination_repr

module Contract = struct
  include Contract_repr
  include Contract_storage

  let is_manager_key_revealed = Contract_manager_storage.is_manager_key_revealed

  let reveal_manager_key = Contract_manager_storage.reveal_manager_key

  let get_manager_key = Contract_manager_storage.get_manager_key

  module Internal_for_tests = Contract_repr
end

module Tx_rollup_level = Tx_rollup_level_repr
module Tx_rollup_commitment_hash = Tx_rollup_commitment_repr.Hash
module Tx_rollup_message_result_hash = Tx_rollup_message_result_hash_repr

module Tx_rollup = struct
  include Tx_rollup_repr
  include Tx_rollup_storage
  module Internal_for_tests = Tx_rollup_repr
end

module Tx_rollup_state = struct
  include Tx_rollup_state_repr
  include Tx_rollup_state_storage

  module Internal_for_tests = struct
    include Tx_rollup_state_repr
    include Tx_rollup_state_repr.Internal_for_tests
  end
end

module Tx_rollup_withdraw = Tx_rollup_withdraw_repr
module Tx_rollup_withdraw_list_hash = Tx_rollup_withdraw_list_hash_repr
module Tx_rollup_message_result = Tx_rollup_message_result_repr

module Tx_rollup_reveal = struct
  include Tx_rollup_reveal_repr
  include Tx_rollup_reveal_storage
end

module Tx_rollup_message = struct
  include Tx_rollup_message_repr

  let make_message msg = (msg, size msg)

  let make_batch string = make_message @@ Batch string

  let make_deposit sender destination ticket_hash amount =
    make_message @@ Deposit {sender; destination; ticket_hash; amount}
end

module Tx_rollup_message_hash = Tx_rollup_message_hash_repr

module Tx_rollup_inbox = struct
  include Tx_rollup_inbox_repr
  include Tx_rollup_inbox_storage
end

module Tx_rollup_commitment = struct
  include Tx_rollup_commitment_repr
  include Tx_rollup_commitment_storage
end

module Tx_rollup_hash = Tx_rollup_hash_builder
module Tx_rollup_errors = Tx_rollup_errors_repr
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

  let list_values ?offset ?length c m =
    Storage.Big_map.Contents.list_values ?offset ?length (c, m)

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

module Delegate = struct
  include Delegate_storage

  type deposits = Storage.deposits = {
    initial_amount : Tez.t;
    current_amount : Tez.t;
  }

  let last_cycle_before_deactivation =
    Delegate_activation_storage.last_cycle_before_deactivation

  let prepare_stake_distribution = Stake_storage.prepare_stake_distribution

  let registered = Contract_delegate_storage.registered

  let find = Contract_delegate_storage.find

  let delegated_contracts = Contract_delegate_storage.delegated_contracts
end

module Stake_distribution = struct
  let snapshot = Stake_storage.snapshot

  let compute_snapshot_index = Delegate_storage.compute_snapshot_index

  let baking_rights_owner = Delegate.baking_rights_owner

  let slot_owner = Delegate.slot_owner

  let delegate_pubkey = Delegate.pubkey

  let get_staking_balance = Delegate.staking_balance
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

  let load_grand_parent_branch ctxt =
    Storage.Tenderbake.Grand_parent_branch.find ctxt >>=? function
    | Some grand_parent_branch ->
        Raw_context.Consensus.set_grand_parent_branch ctxt grand_parent_branch
        |> return
    | None -> return ctxt

  let store_grand_parent_branch ctxt branch =
    let ctxt = set_grand_parent_branch ctxt branch in
    Storage.Tenderbake.Grand_parent_branch.add ctxt branch
end

let prepare_first_block = Init_storage.prepare_first_block

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  Init_storage.prepare ctxt ~level ~predecessor_timestamp ~timestamp
  >>=? fun (ctxt, balance_updates, origination_results) ->
  Consensus.load_endorsement_branch ctxt >>=? fun ctxt ->
  Consensus.load_grand_parent_branch ctxt >>=? fun ctxt ->
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

let activate = Raw_context.activate

let reset_internal_nonce = Raw_context.reset_internal_nonce

let fresh_internal_nonce = Raw_context.fresh_internal_nonce

let record_internal_nonce = Raw_context.record_internal_nonce

let internal_nonce_already_recorded =
  Raw_context.internal_nonce_already_recorded

let description = Raw_context.description

module Parameters = Parameters_repr

module Liquidity_baking = struct
  include Liquidity_baking_repr
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
