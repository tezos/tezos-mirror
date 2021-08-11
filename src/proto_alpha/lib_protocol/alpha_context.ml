(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

module Raw_level = Raw_level_repr
module Cycle = Cycle_repr
module Script_string = Script_string_repr
module Script_int = Script_int_repr

module Script_timestamp = struct
  include Script_timestamp_repr

  let now ctxt =
    let {Constants_repr.minimal_block_delay; _} = Raw_context.constants ctxt in
    let current_timestamp = Raw_context.predecessor_timestamp ctxt in
    Time.add current_timestamp (Period_repr.to_seconds minimal_block_delay)
    |> Timestamp.to_seconds |> of_int64
end

module Script = struct
  include Michelson_v1_primitives
  include Script_repr

  let force_decode_in_context ctxt lexpr =
    Raw_context.consume_gas ctxt (Script_repr.force_decode_cost lexpr)
    >>? fun ctxt ->
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
end

module Voting_period = struct
  include Voting_period_repr
  include Voting_period_storage
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

  let gas_exhausted_error = Raw_context.gas_exhausted_error

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

  let legacy_big_map_diff_encoding =
    Data_encoding.conv
      Contract_storage.Legacy_big_map_diff.of_lazy_storage_diff
      Contract_storage.Legacy_big_map_diff.to_lazy_storage_diff
      Contract_storage.Legacy_big_map_diff.encoding
end

module Contract = struct
  include Contract_repr
  include Contract_storage

  let originate c contract ~balance ~script ~delegate =
    raw_originate c contract ~balance ~script ~delegate

  let init_origination_nonce = Raw_context.init_origination_nonce

  let unset_origination_nonce = Raw_context.unset_origination_nonce
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
end

module Receipt = Receipt_repr
module Delegate = Delegate_storage

module Roll = struct
  include Roll_repr
  include Roll_storage
end

module Nonce = Nonce_storage

module Seed = struct
  include Seed_repr
  include Seed_storage
end

module Fitness = struct
  include Fitness_repr
  include Fitness

  type fitness = t

  include Fitness_storage
end

module Bootstrap = Bootstrap_storage

module Commitment = struct
  include Commitment_repr
  include Commitment_storage
end

module Global = struct
  let get_block_priority = Storage.Block_priority.get

  let set_block_priority = Storage.Block_priority.update
end

module Migration = Migration_repr

let prepare_first_block = Init_storage.prepare_first_block

let prepare = Init_storage.prepare

(* The rationale behind the value of this constant is that an
   operation should be considered as alive for about one hour:

   minimal_block_delay context *  max_operations_ttl = 3600

   To avoid an unecessary computation, we have hard-coded the value of
   this constant.  *)
let max_operations_ttl = 120

let finalize ?commit_message:message c =
  let fitness = Fitness.from_int64 (Fitness.current c) in
  let context = Raw_context.recover c in
  {
    Updater.context;
    fitness;
    message;
    max_operations_ttl;
    last_allowed_fork_level =
      Raw_level.to_int32 @@ Level.last_allowed_fork_level c;
  }

let activate = Raw_context.activate

let record_endorsement = Raw_context.record_endorsement

let allowed_endorsements = Raw_context.allowed_endorsements

let init_endorsements = Raw_context.init_endorsements

let included_endorsements = Raw_context.included_endorsements

let reset_internal_nonce = Raw_context.reset_internal_nonce

let fresh_internal_nonce = Raw_context.fresh_internal_nonce

let record_internal_nonce = Raw_context.record_internal_nonce

let internal_nonce_already_recorded =
  Raw_context.internal_nonce_already_recorded

let add_deposit = Raw_context.add_deposit

let add_fees = Raw_context.add_fees

let add_rewards = Raw_context.add_rewards

let get_deposits = Raw_context.get_deposits

let get_fees = Raw_context.get_fees

let get_rewards = Raw_context.get_rewards

let description = Raw_context.description

module Parameters = Parameters_repr
module Liquidity_baking = Liquidity_baking_repr

module Cache = struct
  type identifier = {namespace : string; id : string}

  let separator = '@'

  let sanitize namespace =
    if String.contains namespace separator then
      invalid_arg
        (Format.asprintf
           "Invalid cache namespace: '%s'. Character %c is forbidden."
           namespace
           separator)
    else namespace

  let string_of_identifier {namespace; id} =
    let (namespace : string) = (namespace :> string) in
    namespace ^ String.make 1 separator ^ id

  let identifier_of_string raw =
    match String.split_on_char '@' raw with
    | [] -> assert false
    | namespace :: id ->
        {namespace = sanitize namespace; id = String.concat "" id}

  let pp_identifier fmt {namespace; id} =
    Format.fprintf fmt "%s%c%s" (namespace :> string) separator id

  let identifier_encoding =
    let open Data_encoding in
    conv_with_guard
      (fun {namespace; id} -> ((namespace :> string), id))
      (fun (namespace, id) ->
        try Ok {namespace = sanitize namespace; id}
        with Invalid_argument s -> Error s)
      (obj2
         (req "namespace" Data_encoding.string)
         (req "id" Data_encoding.string))

  type key_maker = KeyMaker of (id:string -> Context.Cache.key)

  include Raw_context.Cache

  let identifier_of_key key =
    let raw = Raw_context.Cache.identifier_of_key key in
    identifier_of_string raw

  let key_of_identifier ~cache_index identifier =
    let raw = string_of_identifier identifier in
    Raw_context.Cache.key_of_identifier ~cache_index raw

  let key_maker =
    let namespaces = ref [] in
    fun ~cache_index ~namespace ->
      let namespace = sanitize namespace in
      if List.mem ~equal:String.equal namespace !namespaces then
        invalid_arg
          (Format.sprintf
             "Cache key namespace %s already exist."
             (namespace :> string))
      else (
        namespaces := namespace :: !namespaces ;
        KeyMaker
          (fun ~id ->
            let identifier = {namespace; id} in
            key_of_identifier ~cache_index identifier))

  let list_keys context ~cache_index =
    Raw_context.Cache.list_keys context ~cache_index
    |> List.map (fun (raw, size) -> (identifier_of_string raw, size))

  let key_rank context key = Raw_context.Cache.key_rank context key
end
