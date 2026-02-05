(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Storage_functors
open Storage_sigs

module Encoding = struct
  module UInt16 : VALUE with type t = int = struct
    type t = int

    let encoding = Data_encoding.uint16
  end

  module Int32 : VALUE with type t = Int32.t = struct
    type t = Int32.t

    let encoding = Data_encoding.int32
  end

  module Int64 : VALUE with type t = Int64.t = struct
    type t = Int64.t

    let encoding = Data_encoding.int64
  end

  module Z : VALUE with type t = Z.t = struct
    type t = Z.t

    let encoding = Data_encoding.z
  end

  module N : VALUE with type t = Z.t = struct
    type t = Z.t

    let encoding = Data_encoding.n
  end

  module Manager_counter : VALUE with type t = Manager_counter_repr.t = struct
    type t = Manager_counter_repr.t

    let encoding = Manager_counter_repr.encoding_for_storage
  end
end

module Int31_index : INDEX with type t = int = struct
  type t = int

  let path_length = 1

  let to_path c l = string_of_int c :: l

  let of_path = function [] | _ :: _ :: _ -> None | [c] -> int_of_string_opt c

  type 'a ipath = 'a * t

  let args =
    Storage_description.One
      {
        rpc_arg = RPC_arg.int;
        encoding = Data_encoding.int31;
        compare = Compare.Int.compare;
      }
end

module Make_index (H : Storage_description.INDEX) :
  INDEX with type t = H.t and type 'a ipath = 'a * H.t = struct
  include H

  type 'a ipath = 'a * t

  let args = Storage_description.One {rpc_arg; encoding; compare}
end

module type Simple_single_data_storage = sig
  type value

  val get : Raw_context.t -> value tzresult Lwt.t

  val update : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t

  val init : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t
end

module Block_round : Simple_single_data_storage with type value = Round_repr.t =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["block_round"]
    end)
    (Round_repr)

module Consecutive_round_zero :
  Simple_single_data_storage with type value = Int32.t =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["consecutive_round_zero"]
    end)
    (Encoding.Int32)

module Protocol_activation_level =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["protocol_activation_level"]
    end)
    (Raw_level_repr)

module Tenderbake = struct
  module First_level_of_protocol =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["first_level_of_protocol"]
      end)
      (Raw_level_repr)

  module Branch = struct
    type t = Block_hash.t * Block_payload_hash.t

    let encoding =
      Data_encoding.(
        obj2
          (req "grand_parent_hash" Block_hash.encoding)
          (req "predecessor_payload" Block_payload_hash.encoding))
  end

  module Attestation_branch =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["attestation_branch"]
      end)
      (Branch)

  module Forbidden_delegates =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["forbidden_delegates"]
      end)
      (Signature.Public_key_hash.Set)
end

(** Contracts handling *)

type missed_attestations_info = {remaining_slots : int; missed_levels : int}

module Missed_attestations_info = struct
  type t = missed_attestations_info

  let encoding =
    let open Data_encoding in
    conv
      (fun {remaining_slots; missed_levels} -> (remaining_slots, missed_levels))
      (fun (remaining_slots, missed_levels) -> {remaining_slots; missed_levels})
      (obj2 (req "remaining_slots" int31) (req "missed_levels" int31))
end

type dal_delegate_participation = {attested_slots : int; attestable_slots : int}

module Dal_delegate_participation = struct
  type t = dal_delegate_participation

  let encoding =
    let open Data_encoding in
    conv
      (fun {attested_slots; attestable_slots} ->
        (attested_slots, attestable_slots))
      (fun (attested_slots, attestable_slots) ->
        {attested_slots; attestable_slots})
      (obj2 (req "attested_slots" int31) (req "attestable_slots" int31))
end

(* TODO #6918: move closer to its only use left after P *)
module Slashed_deposits_history = struct
  type slashed_percentage = Percentage.t

  (* invariant: sorted list *)
  type t = (Cycle_repr.t * slashed_percentage) list

  let encoding =
    let open Data_encoding in
    list
      (obj2
         (req "cycle" Cycle_repr.encoding)
         (req "slashed_percentage" Percentage.encoding))

  let add cycle percentage history =
    let rec loop rev_prefix = function
      | (c, p) :: tl when Cycle_repr.(cycle = c) ->
          let p = Percentage.add_bounded p percentage in
          (* cycle found, do not change the order *)
          List.rev_append rev_prefix ((c, p) :: tl)
      | ((c, _) as hd) :: tl when Cycle_repr.(cycle > c) ->
          (* [cycle] must be inserted later *)
          loop (hd :: rev_prefix) tl
      | suffix ->
          (* [cycle] between [rev_prefix] and [suffix] *)
          List.rev_append rev_prefix ((cycle, percentage) :: suffix)
    in
    loop [] history

  let rec get cycle = function
    | (c, p) :: _ when Cycle_repr.(cycle = c) -> p
    | (c, _) :: tl when Cycle_repr.(cycle > c) -> get cycle tl
    | _ -> Percentage.p0
end

(* TODO #6918: Remove after P *)
module Slashed_deposits_history__Oxford = struct
  include Slashed_deposits_history

  let encoding =
    let open Data_encoding in
    list
      (obj2
         (req "cycle" Cycle_repr.encoding)
         (req "slashed_percentage" Percentage.encoding_legacy_in_o))
end

module Unstake_request = struct
  type request = Cycle_repr.t * Tez_repr.t

  type requests = request list

  type t = {delegate : Signature.Public_key_hash.t; requests : requests}

  let request_encoding =
    let open Data_encoding in
    obj2
      (req "cycle" Cycle_repr.encoding)
      (req "requested_amount" Tez_repr.encoding)

  let requests_encoding = Data_encoding.list request_encoding

  let encoding =
    let open Data_encoding in
    conv
      (fun {delegate; requests} -> (delegate, requests))
      (fun (delegate, requests) -> {delegate; requests})
      (obj2
         (req "delegate" Contract_repr.implicit_encoding)
         (req "requests" requests_encoding))

  let add cycle amount requests =
    let open Result_syntax in
    if Tez_repr.(amount > zero) then
      let rec loop rev_prefix = function
        | [] ->
            (* cycle does not exist -> add at the head *)
            Ok ((cycle, amount) :: requests)
        | (c, a) :: tl when Cycle_repr.(c = cycle) ->
            let+ a = Tez_repr.(a +? amount) in
            (* cycle found, do not change the order *)
            List.rev_append rev_prefix ((c, a) :: tl)
        | hd :: tl -> loop (hd :: rev_prefix) tl
      in
      loop [] requests
    else return requests
end

module Contract = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["contracts"]
      end)

  module Global_counter :
    Simple_single_data_storage with type value = Manager_counter_repr.t =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["global_counter"]
      end)
      (Encoding.Manager_counter)

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Contract_repr.Index))

  let fold = Indexed_context.fold_keys

  let list = Indexed_context.keys

  type local_context = Indexed_context.local_context

  let with_local_context = Indexed_context.with_local_context

  module Spendable_balance =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["balance"]
      end)
      (Tez_repr)

  module Missed_attestations =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["missed_attestations"]
      end)
      (Missed_attestations_info)

  module Dal_participation =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["dal_delegate_participation"]
      end)
      (Dal_delegate_participation)

  module Manager =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["manager"]
      end)
      (Manager_repr)

  module Consensus_key =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["consensus_key"; "active"]
      end)
      (Signature.Public_key)

  module Companion_key =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["companion_key"; "active"]
      end)
      (Bls.Public_key)

  module Staking_parameters =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["staking_parameters"; "active"]
      end)
      (Staking_parameters_repr)

  module Delegate =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["delegate"]
      end)
      (Signature.Public_key_hash)

  module Inactive_delegate =
    Indexed_context.Make_set
      (Registered)
      (struct
        let name = ["inactive_delegate"]
      end)

  module Delegate_last_cycle_before_deactivation =
    Indexed_context.Make_map
      (Registered)
      (struct
        (* FIXME? Change the key name to reflect the functor's name *)
        let name = ["delegate_desactivation"]
      end)
      (Cycle_repr)

  module Delegated =
    Make_data_set_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["delegated"]
         end))
         (Make_index (Contract_repr.Index))

  module SWRR_credit =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["swrr_credit"]
      end)
      (struct
        type t = Z.t

        let encoding = Data_encoding.z
      end)

  module Counter =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["counter"]
      end)
      (Encoding.Manager_counter)

  (* Consume gas for serialization and deserialization of expr in this
     module *)
  module Make_carbonated_map_expr (N : Storage_sigs.NAME) :
    Storage_sigs.Non_iterable_indexed_carbonated_data_storage
      with type key = Contract_repr.t
       and type value = Script_repr.lazy_expr
       and type t := Raw_context.t = struct
    module I =
      Indexed_context.Make_carbonated_map (Registered) (N)
        (struct
          type t = Script_repr.lazy_expr

          let encoding = Script_repr.lazy_expr_encoding
        end)

    type context = I.context

    type key = I.key

    type value = I.value

    let mem = I.mem

    let remove_existing = I.remove_existing

    let remove = I.remove

    let consume_deserialize_gas ctxt value =
      Raw_context.consume_gas ctxt (Script_repr.force_decode_cost value)

    let consume_serialize_gas ctxt value =
      Raw_context.consume_gas ctxt (Script_repr.force_bytes_cost value)

    let get ctxt contract =
      let open Lwt_result_syntax in
      let* ctxt, value = I.get ctxt contract in
      let*? ctxt = consume_deserialize_gas ctxt value in
      return (ctxt, value)

    let find ctxt contract =
      let open Lwt_result_syntax in
      let* ctxt, value_opt = I.find ctxt contract in
      match value_opt with
      | None -> return (ctxt, None)
      | Some value ->
          let*? ctxt = consume_deserialize_gas ctxt value in
          return (ctxt, value_opt)

    let update ctxt contract value =
      let open Lwt_result_syntax in
      let*? ctxt = consume_serialize_gas ctxt value in
      I.update ctxt contract value

    let add_or_remove ctxt contract value_opt =
      let open Lwt_result_syntax in
      match value_opt with
      | None -> I.add_or_remove ctxt contract None
      | Some value ->
          let*? ctxt = consume_serialize_gas ctxt value in
          I.add_or_remove ctxt contract value_opt

    let init ctxt contract value =
      let open Lwt_result_syntax in
      let*? ctxt = consume_serialize_gas ctxt value in
      I.init ctxt contract value

    let add ctxt contract value =
      let open Lwt_result_syntax in
      let*? ctxt = consume_serialize_gas ctxt value in
      I.add ctxt contract value

    let keys_unaccounted = I.keys_unaccounted
  end

  module Native =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["native"]
      end)
      (Script_native_repr)

  module Code = Make_carbonated_map_expr (struct
    let name = ["code"]
  end)

  module Storage = Make_carbonated_map_expr (struct
    let name = ["storage"]
  end)

  module Paid_storage_space =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["paid_bytes"]
      end)
      (Encoding.Z)

  module Used_storage_space =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["used_bytes"]
      end)
      (Encoding.Z)

  module Unstaked_frozen_deposits =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["unstaked_frozen_deposits"]
      end)
      (Unstaked_frozen_deposits_repr)

  module Unstake_requests =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["unstake_requests"]
      end)
      (Unstake_request)

  module Frozen_deposits_pseudotokens =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["frozen_deposits_pseudotokens"]
      end)
      (Staking_pseudotoken_repr)

  module Staking_pseudotokens =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["staking_pseudotokens"]
      end)
      (Staking_pseudotoken_repr)

  module Frozen_deposits_limit =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["frozen_deposits_limit"]
      end)
      (Tez_repr)

  (* TODO #6918: Remove after P *)
  module Slashed_deposits__Oxford =
    Indexed_context.Make_map
      (Ghost)
      (struct
        let name = ["slashed_deposits"]
      end)
      (Slashed_deposits_history__Oxford)

  module Bond_id_index =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["bond_id_index"]
         end))
         (Make_index (Bond_id_repr.Index))

  module Frozen_bonds =
    Bond_id_index.Make_carbonated_map
      (Registered)
      (struct
        let name = ["frozen_bonds"]
      end)
      (Tez_repr)

  let fold_bond_ids = Bond_id_index.fold_keys

  module Total_frozen_bonds =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["total_frozen_bonds"]
      end)
      (Tez_repr)

  module Total_supply =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["total_supply"]
      end)
      (Tez_repr)

  module Address_registry = struct
    module Raw_context =
      Make_subcontext (Registered) (Raw_context)
        (struct
          let name = ["address_registry"]
        end)

    module Next =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["next"]
        end)
        (Encoding.N)

    module Registry =
      Make_indexed_carbonated_data_storage
        (Make_subcontext (Registered) (Raw_context)
           (struct
             let name = ["addresses"]
           end))
           (Make_index (Destination_repr.Index))
        (Encoding.N)
  end

  module Native_contracts = struct
    module Raw_context =
      Make_subcontext (Registered) (Raw_context)
        (struct
          let name = ["native_contracts"]
        end)

    module CLST =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["clst"]
        end)
        (struct
          type t = Contract_hash.t

          let encoding = Contract_repr.originated_encoding
        end)
  end
end

module type NEXT = sig
  type id

  val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

  val incr : Raw_context.t -> (Raw_context.t * id) tzresult Lwt.t
end

module Global_constants = struct
  module Map :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t
       and type key = Script_expr_hash.t
       and type value = Script_repr.expr =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["global_constant"]
         end))
         (Make_index (Script_expr_hash))
      (struct
        type t = Script_repr.expr

        let encoding = Script_repr.expr_encoding
      end)
end

(** Big maps handling *)

module Big_map = struct
  type id = Lazy_storage_kind.Big_map.Id.t

  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["big_maps"]
      end)

  module Next : NEXT with type id := id = struct
    module Storage =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["next"]
        end)
        (Lazy_storage_kind.Big_map.Id)

    let incr ctxt =
      let open Lwt_result_syntax in
      let* i = Storage.get ctxt in
      let* ctxt = Storage.update ctxt (Lazy_storage_kind.Big_map.Id.next i) in
      return (ctxt, i)

    let init ctxt = Storage.init ctxt Lazy_storage_kind.Big_map.Id.init
  end

  module Index = Lazy_storage_kind.Big_map.Id

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Index))

  let rpc_arg = Index.rpc_arg

  let fold = Indexed_context.fold_keys

  let list = Indexed_context.keys

  let remove ctxt n = Indexed_context.remove ctxt n

  let copy ctxt ~from ~to_ = Indexed_context.copy ctxt ~from ~to_

  type key = Raw_context.t * Index.t

  module Total_bytes =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["total_bytes"]
      end)
      (Encoding.Z)

  module Key_type =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["key_type"]
      end)
      (struct
        type t = Script_repr.expr

        let encoding = Script_repr.expr_encoding
      end)

  module Value_type =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["value_type"]
      end)
      (struct
        type t = Script_repr.expr

        let encoding = Script_repr.expr_encoding
      end)

  module Contents = struct
    module I =
      Storage_functors.Make_indexed_carbonated_data_storage
        (Make_subcontext (Registered) (Indexed_context.Raw_context)
           (struct
             let name = ["contents"]
           end))
           (Make_index (Script_expr_hash))
        (struct
          type t = Script_repr.expr

          let encoding = Script_repr.expr_encoding
        end)

    type context = I.context

    type key = I.key

    type value = I.value

    let mem = I.mem

    let remove_existing = I.remove_existing

    let remove = I.remove

    let update = I.update

    let add_or_remove = I.add_or_remove

    let init = I.init

    let add = I.add

    let list_key_values = I.list_key_values

    let consume_deserialize_gas ctxt value =
      Raw_context.consume_gas ctxt (Script_repr.deserialized_cost value)

    let get ctxt contract =
      let open Lwt_result_syntax in
      let* ctxt, value = I.get ctxt contract in
      let*? ctxt = consume_deserialize_gas ctxt value in
      return (ctxt, value)

    let find ctxt contract =
      let open Lwt_result_syntax in
      let* ctxt, value_opt = I.find ctxt contract in
      match value_opt with
      | None -> return (ctxt, None)
      | Some value ->
          let*? ctxt = consume_deserialize_gas ctxt value in
          return (ctxt, value_opt)

    let keys_unaccounted = I.keys_unaccounted
  end
end

module Sapling = struct
  type id = Lazy_storage_kind.Sapling_state.Id.t

  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["sapling"]
      end)

  module Next = struct
    module Storage =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["next"]
        end)
        (Lazy_storage_kind.Sapling_state.Id)

    let incr ctxt =
      let open Lwt_result_syntax in
      let* i = Storage.get ctxt in
      let* ctxt =
        Storage.update ctxt (Lazy_storage_kind.Sapling_state.Id.next i)
      in
      return (ctxt, i)

    let init ctxt = Storage.init ctxt Lazy_storage_kind.Sapling_state.Id.init
  end

  module Index = Lazy_storage_kind.Sapling_state.Id

  let rpc_arg = Index.rpc_arg

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Index))

  let remove ctxt n = Indexed_context.remove ctxt n

  let copy ctxt ~from ~to_ = Indexed_context.copy ctxt ~from ~to_

  module Total_bytes =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["total_bytes"]
      end)
      (Encoding.Z)

  module Commitments_size =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["commitments_size"]
      end)
      (Encoding.Int64)

  module Memo_size =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["memo_size"]
      end)
      (Sapling_repr.Memo_size)

  module Commitments :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Hash.t =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitments"]
         end))
         (Make_index (struct
           type t = int64

           let rpc_arg =
             let construct = Int64.to_string in
             let destruct hash =
               Int64.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse node position"
             in
             RPC_arg.make
               ~descr:"The position of a node in a sapling commitment tree"
               ~name:"sapling_node_position"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_node_position"
               ~title:"Sapling node position"
               ~description:
                 "The position of a node in a sapling commitment tree"
               Data_encoding.int64

           let compare = Compare.Int64.compare

           let path_length = 1

           let to_path c l = Int64.to_string c :: l

           let of_path = function [c] -> Int64.of_string_opt c | _ -> None
         end))
      (Sapling.Hash)

  let commitments_init ctx id =
    let open Lwt_syntax in
    let+ ctx, (_id : id) =
      Indexed_context.Raw_context.remove (ctx, id) ["commitments"]
    in
    ctx

  module Ciphertexts :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Ciphertext.t =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["ciphertexts"]
         end))
         (Make_index (struct
           type t = int64

           let rpc_arg =
             let construct = Int64.to_string in
             let destruct hash =
               Int64.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse ciphertext position"
             in
             RPC_arg.make
               ~descr:"The position of a sapling ciphertext"
               ~name:"sapling_ciphertext_position"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_ciphertext_position"
               ~title:"Sapling ciphertext position"
               ~description:"The position of a sapling ciphertext"
               Data_encoding.int64

           let compare = Compare.Int64.compare

           let path_length = 1

           let to_path c l = Int64.to_string c :: l

           let of_path = function [c] -> Int64.of_string_opt c | _ -> None
         end))
      (Sapling.Ciphertext)

  let ciphertexts_init ctx id =
    let open Lwt_syntax in
    let+ ctx, (_id : id) =
      Indexed_context.Raw_context.remove (ctx, id) ["ciphertexts"]
    in
    ctx

  module Nullifiers_size =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["nullifiers_size"]
      end)
      (Encoding.Int64)

  (* For sequential access when building a diff *)
  module Nullifiers_ordered :
    Non_iterable_indexed_data_storage
      with type t := Raw_context.t * id
       and type key = int64
       and type value = Sapling.Nullifier.t =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["nullifiers_ordered"]
         end))
         (Make_index (struct
           type t = int64

           let rpc_arg =
             let construct = Int64.to_string in
             let destruct hash =
               Int64.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse nullifier position"
             in
             RPC_arg.make
               ~descr:"A sapling nullifier position"
               ~name:"sapling_nullifier_position"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_nullifier_position"
               ~title:"Sapling nullifier position"
               ~description:"Sapling nullifier position"
               Data_encoding.int64

           let compare = Compare.Int64.compare

           let path_length = 1

           let to_path c l = Int64.to_string c :: l

           let of_path = function [c] -> Int64.of_string_opt c | _ -> None
         end))
      (Sapling.Nullifier)

  (* Check membership in O(1) for verify_update *)
  module Nullifiers_hashed =
    Make_carbonated_data_set_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["nullifiers_hashed"]
         end))
         (Make_index (struct
           type t = Sapling.Nullifier.t

           let encoding = Sapling.Nullifier.encoding

           let of_string hexstring =
             Option.bind
               (Hex.to_bytes (`Hex hexstring))
               (Data_encoding.Binary.of_bytes_opt encoding)
             |> Result.of_option ~error:"Cannot parse sapling nullifier"

           let to_string nf =
             let b = Data_encoding.Binary.to_bytes_exn encoding nf in
             let (`Hex hexstring) = Hex.of_bytes b in
             hexstring

           let rpc_arg =
             RPC_arg.make
               ~descr:"A sapling nullifier"
               ~name:"sapling_nullifier"
               ~construct:to_string
               ~destruct:of_string
               ()

           let compare = Sapling.Nullifier.compare

           let path_length = 1

           let to_path c l = to_string c :: l

           let of_path = function
             | [c] -> Result.to_option (of_string c)
             | _ -> None
         end))

  let nullifiers_init ctx id =
    let open Lwt_syntax in
    let* ctx = Nullifiers_size.add (ctx, id) Int64.zero in
    let* ctx, id =
      Indexed_context.Raw_context.remove (ctx, id) ["nullifiers_ordered"]
    in
    let+ ctx, (_id : id) =
      Indexed_context.Raw_context.remove (ctx, id) ["nullifiers_hashed"]
    in
    ctx

  module Roots :
    Non_iterable_indexed_data_storage
      with type t := Raw_context.t * id
       and type key = int32
       and type value = Sapling.Hash.t =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["roots"]
         end))
         (Make_index (struct
           type t = int32

           let rpc_arg =
             let construct = Int32.to_string in
             let destruct hash =
               Int32.of_string_opt hash
               |> Result.of_option ~error:"Cannot parse nullifier position"
             in
             RPC_arg.make
               ~descr:"A sapling root"
               ~name:"sapling_root"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "sapling_root"
               ~title:"Sapling root"
               ~description:"Sapling root"
               Data_encoding.int32

           let compare = Compare.Int32.compare

           let path_length = 1

           let to_path c l = Int32.to_string c :: l

           let of_path = function [c] -> Int32.of_string_opt c | _ -> None
         end))
      (Sapling.Hash)

  module Roots_pos =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["roots_pos"]
      end)
      (Encoding.Int32)

  module Roots_level =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["roots_level"]
      end)
      (Raw_level_repr)
end

module Public_key_hash = struct
  open Signature
  include Signature.Public_key_hash
  module Path_Ed25519 = Path_encoding.Make_hex (Ed25519.Public_key_hash)
  module Path_Secp256k1 = Path_encoding.Make_hex (Secp256k1.Public_key_hash)
  module Path_P256 = Path_encoding.Make_hex (P256.Public_key_hash)
  module Path_Bls = Path_encoding.Make_hex (Bls.Public_key_hash)
  module Path_Mldsa44 = Path_encoding.Make_hex (Mldsa44.Public_key_hash)

  let to_path (key : public_key_hash) l =
    match key with
    | Ed25519 h -> "ed25519" :: Path_Ed25519.to_path h l
    | Secp256k1 h -> "secp256k1" :: Path_Secp256k1.to_path h l
    | P256 h -> "p256" :: Path_P256.to_path h l
    | Bls h -> "bls" :: Path_Bls.to_path h l
    | Mldsa44 h -> "mldsa44" :: Path_Mldsa44.to_path h l

  let of_path : _ -> public_key_hash option = function
    | "ed25519" :: rest -> (
        match Path_Ed25519.of_path rest with
        | Some pkh -> Some (Ed25519 pkh)
        | None -> None)
    | "secp256k1" :: rest -> (
        match Path_Secp256k1.of_path rest with
        | Some pkh -> Some (Secp256k1 pkh)
        | None -> None)
    | "p256" :: rest -> (
        match Path_P256.of_path rest with
        | Some pkh -> Some (P256 pkh)
        | None -> None)
    | "bls" :: rest -> (
        match Path_Bls.of_path rest with
        | Some pkh -> Some (Bls pkh)
        | None -> None)
    | "mldsa44" :: rest -> (
        match Path_Mldsa44.of_path rest with
        | Some pkh -> Some (Mldsa44 pkh)
        | None -> None)
    | _ -> None

  let path_length =
    let l1 = Path_Ed25519.path_length
    and l2 = Path_Secp256k1.path_length
    and l3 = Path_P256.path_length
    and l4 = Path_Bls.path_length
    and l5 = Path_Mldsa44.path_length in
    assert (Compare.Int.(l1 = l2 && l2 = l3 && l3 = l4 && l4 = l5)) ;
    l1 + 1
end

module Public_key_hash_index = Make_index (Public_key_hash)

module Protocol_hash_with_path_encoding = struct
  include Protocol_hash
  include Path_encoding.Make_hex (Protocol_hash)
end

module Delegates =
  Make_data_set_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["delegates"]
       end))
       (Public_key_hash_index)

module Consensus_keys =
  Make_data_set_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["consensus_keys"]
       end))
       (Public_key_hash_index)

module Pending_denunciations =
  Make_indexed_data_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["denunciations"]
       end))
       (Public_key_hash_index)
    (Denunciations_repr)

module Slashed_deposits =
  Make_indexed_data_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["slashed_deposits"]
       end))
       (Public_key_hash_index)
    (Slashed_deposits_history)

(** Per cycle storage *)

type denounced = {
  for_double_preattesting : bool;
  for_double_attesting : bool;
  for_double_baking : bool;
}

let default_denounced =
  {
    for_double_preattesting = false;
    for_double_attesting = false;
    for_double_baking = false;
  }

module Cycle = struct
  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["cycle"]
         end))
         (Make_index (Cycle_repr.Index))

  module Already_denounced =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["already_denounced"]
         end))
         (Pair
            (Pair
               (Make_index
                  (Raw_level_repr.Index))
                  (Make_index (Round_repr.Index)))
               (Public_key_hash_index))
      (struct
        type t = denounced

        let encoding =
          let open Data_encoding in
          conv
            (fun {
                   for_double_preattesting;
                   for_double_attesting;
                   for_double_baking;
                 }
               ->
              (for_double_preattesting, for_double_attesting, for_double_baking))
            (fun ( for_double_preattesting,
                   for_double_attesting,
                   for_double_baking )
               ->
              {for_double_preattesting; for_double_attesting; for_double_baking})
            (obj3
               (req "for_double_preattesting" bool)
               (req "for_double_attesting" bool)
               (req "for_double_baking" bool))
      end)

  module Dal_already_denounced =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["dal_already_denounced"]
         end))
         (Pair
            (Pair
               (Make_index
                  (Raw_level_repr.Index))
                  (Make_index (Dal_slot_index_repr.Index)))
               (Public_key_hash_index))
      (struct
        type t = unit

        let encoding = Data_encoding.unit
      end)

  module Selected_stake_distribution =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["selected_stake_distribution"]
      end)
      (struct
        type t = (Signature.Public_key_hash.t * Stake_repr.t) list

        let encoding =
          Data_encoding.(
            Variable.list
              (obj2
                 (req "baker" Signature.Public_key_hash.encoding)
                 (req "active_stake" Stake_repr.encoding)))
      end)

  module Selected_bakers =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["selected_bakers"]
      end)
      (struct
        type t = Signature.Public_key_hash.t list

        let encoding =
          Data_encoding.(Variable.list Signature.Public_key_hash.encoding)
      end)

  module Total_active_stake =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["total_active_stake"]
      end)
      (Stake_repr)

  type consensus_pk_in_R = {
    delegate : Signature.Public_key_hash.t;
    consensus_pk : Signature.Public_key.t;
    consensus_pkh : Signature.Public_key_hash.t;
  }

  let consensus_pk_encoding_in_R =
    let open Data_encoding in
    conv
      (fun {delegate; consensus_pk; consensus_pkh} ->
        if Signature.Public_key_hash.equal consensus_pkh delegate then
          (consensus_pk, None)
        else (consensus_pk, Some delegate))
      (fun (consensus_pk, delegate) ->
        let consensus_pkh = Signature.Public_key.hash consensus_pk in
        let delegate =
          match delegate with None -> consensus_pkh | Some del -> del
        in
        {delegate; consensus_pk; consensus_pkh})
      (obj2
         (req "consensus_pk" Signature.Public_key.encoding)
         (opt "delegate" Signature.Public_key_hash.encoding))

  module Delegate_sampler_state_up_to_R =
    Indexed_context.Make_map
      (Ghost)
      (struct
        let name = ["delegate_sampler_state"]
      end)
      (struct
        type t = consensus_pk_in_R Sampler.t

        let encoding = Sampler.encoding consensus_pk_encoding_in_R
      end)

  module Delegate_sampler_state =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["delegate_sampler_state"]
      end)
      (struct
        type t = Raw_context.consensus_pk Sampler.t

        let encoding = Sampler.encoding Raw_context.consensus_pk_encoding
      end)

  module Issuance_bonus =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["issuance_bonus"]
      end)
      (Issuance_bonus_repr)

  module Issuance_coeff =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["issuance_coeff"]
      end)
      (struct
        type t = Q.t

        let encoding =
          Data_encoding.(
            conv_with_guard
              (fun Q.{num; den} -> (num, den))
              (fun (num, den) ->
                if Compare.Z.(num > Z.zero && den > Z.zero) then
                  Ok (Q.make num den)
                else
                  Error
                    "Invalid Reward Coefficient: only positive values allowed")
              (obj2 (req "numerator" n) (req "denominator" n)))
      end)

  type unrevealed_nonce = {
    nonce_hash : Nonce_hash.t;
    delegate : Signature.Public_key_hash.t;
  }

  type nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  let nonce_status_encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Unrevealed"
          (tup2 Nonce_hash.encoding Signature.Public_key_hash.encoding)
          (function
            | Unrevealed {nonce_hash; delegate} -> Some (nonce_hash, delegate)
            | _ -> None)
          (fun (nonce_hash, delegate) -> Unrevealed {nonce_hash; delegate});
        case
          (Tag 1)
          ~title:"Revealed"
          Seed_repr.nonce_encoding
          (function Revealed nonce -> Some nonce | _ -> None)
          (fun nonce -> Revealed nonce);
      ]

  module Nonce =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["nonces"]
         end))
         (Make_index (Raw_level_repr.Index))
      (struct
        type t = nonce_status

        let encoding = nonce_status_encoding
      end)

  module Seed =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["random_seed"]
      end)
      (struct
        type t = Seed_repr.seed

        let encoding = Seed_repr.seed_encoding
      end)

  module Pending_consensus_keys =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["pending_consensus_keys"]
         end))
         (Make_index (Contract_repr.Index))
      (Signature.Public_key)

  module Pending_companion_keys =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["pending_companion_keys"]
         end))
         (Make_index (Contract_repr.Index))
      (Bls.Public_key)

  module Pending_staking_parameters =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["pending_staking_parameters"]
         end))
         (Make_index (Contract_repr.Index))
      (Staking_parameters_repr)
end

module Already_denounced = Cycle.Already_denounced
module Dal_already_denounced = Cycle.Dal_already_denounced
module Pending_consensus_keys = Cycle.Pending_consensus_keys
module Pending_companion_keys = Cycle.Pending_companion_keys
module Pending_staking_parameters = Cycle.Pending_staking_parameters

module Stake = struct
  module Staking_balance =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["staking_balance"]
         end))
         (Public_key_hash_index)
      (Full_staking_balance_repr)

  module Active_delegates_with_minimal_stake =
    Make_data_set_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["active_delegates_with_minimal_stake"]
         end))
         (Public_key_hash_index)

  module Selected_distribution_for_cycle = Cycle.Selected_stake_distribution
  module Total_active_stake = Cycle.Total_active_stake
  module Selected_bakers = Cycle.Selected_bakers
end

module Clst = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["clst"]
      end)

  module Deposits_balance =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["deposits"]
      end)
      (Tez_repr)

  module Redeemed_frozen_deposits =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["redeemed_frozen_deposits"]
      end)
      (Unstaked_frozen_deposits_repr)

  module Redemption_requests =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["redemption_requests"]
         end))
         (Make_index (Contract_repr.Index))
      (struct
        type t = Unstake_request.requests

        let encoding = Unstake_request.requests_encoding
      end)
end

type consensus_pk_in_R = Cycle.consensus_pk_in_R = {
  delegate : Signature.Public_key_hash.t;
  consensus_pk : Signature.Public_key.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

module Delegate_sampler_state_up_to_R = Cycle.Delegate_sampler_state_up_to_R
module Delegate_sampler_state = Cycle.Delegate_sampler_state
module Issuance_bonus = Cycle.Issuance_bonus
module Issuance_coeff = Cycle.Issuance_coeff

(** Votes *)

module Vote = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["votes"]
      end)

  module Pred_period_kind =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["pred_period_kind"]
      end)
      (struct
        type t = Voting_period_repr.kind

        let encoding = Voting_period_repr.kind_encoding
      end)

  module Current_period =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["current_period"]
      end)
      (struct
        type t = Voting_period_repr.t

        let encoding = Voting_period_repr.encoding
      end)

  module Participation_ema =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["participation_ema"]
      end)
      (Encoding.Int32)

  module Current_proposal =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["current_proposal"]
      end)
      (Protocol_hash)

  module Voting_power_in_listings =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["voting_power_in_listings"]
      end)
      (Encoding.Int64)

  module Listings =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["listings"]
         end))
         (Public_key_hash_index)
      (Encoding.Int64)

  module Proposals =
    Make_data_set_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["proposals"]
         end))
         (Pair
            (Make_index
               (Protocol_hash_with_path_encoding))
               (Public_key_hash_index))

  module Proposals_count =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["proposals_count"]
         end))
         (Public_key_hash_index)
      (Encoding.UInt16)

  module Ballots =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["ballots"]
         end))
         (Public_key_hash_index)
      (struct
        type t = Vote_repr.ballot

        let encoding = Vote_repr.ballot_encoding
      end)
end

module type FOR_CYCLE = sig
  val init :
    Raw_context.t ->
    Cycle_repr.t ->
    Seed_repr.seed ->
    Raw_context.t tzresult Lwt.t

  val mem : Raw_context.t -> Cycle_repr.t -> bool Lwt.t

  val get : Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t

  val update :
    Raw_context.t ->
    Cycle_repr.t ->
    Seed_repr.seed ->
    Seed_repr.seed_status ->
    Raw_context.t tzresult Lwt.t

  val remove_existing :
    Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
end

(** Seed *)

module Seed_status =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["seed_status"]
    end)
    (struct
      type t = Seed_repr.seed_status

      let encoding = Seed_repr.seed_status_encoding
    end)

module Seed = struct
  type unrevealed_nonce = Cycle.unrevealed_nonce = {
    nonce_hash : Nonce_hash.t;
    delegate : Signature.Public_key_hash.t;
  }

  type nonce_status = Cycle.nonce_status =
    | Unrevealed of unrevealed_nonce
    | Revealed of Seed_repr.nonce

  module Nonce :
    Non_iterable_indexed_data_storage
      with type key := Level_repr.t
       and type value := nonce_status
       and type t := Raw_context.t = struct
    open Level_repr

    type context = Raw_context.t

    let mem ctxt (l : Level_repr.t) = Cycle.Nonce.mem (ctxt, l.cycle) l.level

    let get ctxt (l : Level_repr.t) = Cycle.Nonce.get (ctxt, l.cycle) l.level

    let find ctxt (l : Level_repr.t) = Cycle.Nonce.find (ctxt, l.cycle) l.level

    let update ctxt (l : Level_repr.t) v =
      Cycle.Nonce.update (ctxt, l.cycle) l.level v

    let init ctxt (l : Level_repr.t) v =
      Cycle.Nonce.init (ctxt, l.cycle) l.level v

    let add ctxt (l : Level_repr.t) v =
      Cycle.Nonce.add (ctxt, l.cycle) l.level v

    let add_or_remove ctxt (l : Level_repr.t) v =
      Cycle.Nonce.add_or_remove (ctxt, l.cycle) l.level v

    let remove_existing ctxt (l : Level_repr.t) =
      Cycle.Nonce.remove_existing (ctxt, l.cycle) l.level

    let remove ctxt (l : Level_repr.t) =
      Cycle.Nonce.remove (ctxt, l.cycle) l.level
  end

  module VDF_setup =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["vdf_challenge"]
      end)
      (struct
        type t = Seed_repr.vdf_setup

        let encoding = Seed_repr.vdf_setup_encoding
      end)

  module For_cycle : FOR_CYCLE = struct
    let init ctxt cycle seed =
      let open Lwt_result_syntax in
      let* ctxt = Cycle.Seed.init ctxt cycle seed in
      let*! ctxt = Seed_status.add ctxt Seed_repr.RANDAO_seed in
      return ctxt

    let mem = Cycle.Seed.mem

    let get = Cycle.Seed.get

    let update ctxt cycle seed status =
      let open Lwt_result_syntax in
      let* ctxt = Cycle.Seed.update ctxt cycle seed in
      Seed_status.update ctxt status

    let remove_existing = Cycle.Seed.remove_existing
  end

  let get_status = Seed_status.get
end

(** Commitments *)

module Commitments =
  Make_indexed_data_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["commitments"]
       end))
       (Make_index (Blinded_public_key_hash.Index))
    (Tez_repr)

(** Ramp up rewards... *)

module Ramp_up = struct
  type reward = {
    baking_reward_fixed_portion : Tez_repr.t;
    baking_reward_bonus_per_slot : Tez_repr.t;
    attesting_reward_per_slot : Tez_repr.t;
    dal_attesting_reward_per_shard : Tez_repr.t;
  }

  module Rewards =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["ramp_up"; "rewards"]
         end))
         (Make_index (Cycle_repr.Index))
      (struct
        type t = reward

        let encoding =
          Data_encoding.(
            conv
              (fun {
                     baking_reward_fixed_portion;
                     baking_reward_bonus_per_slot;
                     attesting_reward_per_slot;
                     dal_attesting_reward_per_shard;
                   }
                 ->
                ( baking_reward_fixed_portion,
                  baking_reward_bonus_per_slot,
                  attesting_reward_per_slot,
                  dal_attesting_reward_per_shard ))
              (fun ( baking_reward_fixed_portion,
                     baking_reward_bonus_per_slot,
                     attesting_reward_per_slot,
                     dal_attesting_reward_per_shard )
                 ->
                {
                  baking_reward_fixed_portion;
                  baking_reward_bonus_per_slot;
                  attesting_reward_per_slot;
                  dal_attesting_reward_per_shard;
                })
              (obj4
                 (req "baking_reward_fixed_portion" Tez_repr.encoding)
                 (req "baking_reward_bonus_per_slot" Tez_repr.encoding)
                 (req "attesting_reward_per_slot" Tez_repr.encoding)
                 (req "dal_attesting_reward_per_shard" Tez_repr.encoding)))
      end)
end

module Pending_migration = struct
  module Balance_updates =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["pending_migration_balance_updates"]
      end)
      (struct
        type t = Receipt_repr.balance_updates

        let encoding = Receipt_repr.balance_updates_encoding
      end)

  module Operation_results =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["pending_migration_operation_results"]
      end)
      (struct
        type t = Migration_repr.origination_result list

        let encoding = Migration_repr.origination_result_list_encoding
      end)

  let remove ctxt =
    let open Lwt_result_syntax in
    let balance_updates ctxt =
      let* balance_updates_opt = Balance_updates.find ctxt in
      match balance_updates_opt with
      | Some balance_updates ->
          let*! ctxt = Balance_updates.remove ctxt in
          (* When applying balance updates in a migration, we must attach receipts.
             The balance updates returned from here will be applied in the first
             block of the new protocol. *)
          return (ctxt, balance_updates)
      | None -> return (ctxt, [])
    in
    let operation_results ctxt =
      let* operation_results_opt = Operation_results.find ctxt in
      match operation_results_opt with
      | Some operation_results ->
          let*! ctxt = Operation_results.remove ctxt in
          return (ctxt, operation_results)
      | None -> return (ctxt, [])
    in
    let* ctxt, balance_updates = balance_updates ctxt in
    let* ctxt, operation_results = operation_results ctxt in
    return (ctxt, balance_updates, operation_results)
end

module Liquidity_baking = struct
  module Toggle_ema =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        (* The old "escape" name is kept here to avoid migrating this. *)
        let name = ["liquidity_baking_escape_ema"]
      end)
      (Encoding.Int32)

  module Cpmm_address =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["liquidity_baking_cpmm_address"]
      end)
      (struct
        type t = Contract_hash.t

        (* Keeping contract-compatible encoding to avoid migrating this. *)
        let encoding = Contract_repr.originated_encoding
      end)
end

module Adaptive_issuance = struct
  module Activation =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["adaptive_issuance_launch_cycle"]
      end)
      (struct
        type t = Cycle_repr.t option

        let encoding = Data_encoding.option Cycle_repr.encoding
      end)
end

module All_bakers_attest_activation =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["all_bakers_attest_first_level"]
    end)
    (struct
      type t = Level_repr.t

      let encoding = Level_repr.encoding
    end)

module Ticket_balance = struct
  module Name = struct
    let name = ["ticket_balance"]
  end

  module Raw_context = Make_subcontext (Registered) (Raw_context) (Name)

  module Paid_storage_space =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["paid_bytes"]
      end)
      (Encoding.Z)

  module Used_storage_space =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["used_bytes"]
      end)
      (Encoding.Z)

  module Table_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["table"]
      end)

  module Index = Make_index (Ticket_hash_repr.Index)
  module Table =
    Make_indexed_carbonated_data_storage (Table_context) (Index) (Encoding.Z)
end

module Sc_rollup = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["smart_rollup"]
      end)

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Sc_rollup_repr.Index))

  module Make_versioned
      (Versioned_value : Sc_rollup_data_version_sig.S)
      (Data_storage : sig
        type context

        type key

        type value = Versioned_value.versioned

        val get : context -> key -> (Raw_context.t * value) tzresult Lwt.t

        val find :
          context -> key -> (Raw_context.t * value option) tzresult Lwt.t

        val update :
          context -> key -> value -> (Raw_context.t * int) tzresult Lwt.t

        val init :
          context -> key -> value -> (Raw_context.t * int) tzresult Lwt.t

        val add :
          context -> key -> value -> (Raw_context.t * int * bool) tzresult Lwt.t

        val add_or_remove :
          context ->
          key ->
          value option ->
          (Raw_context.t * int * bool) tzresult Lwt.t
      end) =
  struct
    include Data_storage

    type value = Versioned_value.t

    let get ctxt key =
      let open Lwt_result_syntax in
      let* ctxt, versioned = get ctxt key in
      return (ctxt, Versioned_value.of_versioned versioned)

    let find ctxt key =
      let open Lwt_result_syntax in
      let* ctxt, versioned = find ctxt key in
      return (ctxt, Option.map Versioned_value.of_versioned versioned)

    let update ctxt key value =
      update ctxt key (Versioned_value.to_versioned value)

    let init ctxt key value = init ctxt key (Versioned_value.to_versioned value)

    let add ctxt key value = add ctxt key (Versioned_value.to_versioned value)

    let add_or_remove ctxt key value =
      add_or_remove ctxt key (Option.map Versioned_value.to_versioned value)
  end

  module PVM_kind =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["kind"]
      end)
      (struct
        type t = Sc_rollups.Kind.t

        let encoding = Sc_rollups.Kind.encoding
      end)

  module Parameters_type =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["parameters_type"]
      end)
      (struct
        type t = Script_repr.lazy_expr

        let encoding = Script_repr.lazy_expr_encoding
      end)

  module Genesis_info =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["genesis_info"]
      end)
      (struct
        type t = Sc_rollup_commitment_repr.genesis_info

        let encoding = Sc_rollup_commitment_repr.genesis_info_encoding
      end)

  module Past_commitment_periods =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["past_commitment_periods"]
      end)
      (struct
        type t = Sc_rollup_repr.Past_commitment_period.t list

        let encoding =
          Data_encoding.(list Sc_rollup_repr.Past_commitment_period.encoding)
      end)

  module Inbox = struct
    include
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["inbox"]
        end)
        (struct
          type t = Sc_rollup_inbox_repr.versioned

          let encoding = Sc_rollup_inbox_repr.versioned_encoding
        end)

    type value = Sc_rollup_inbox_repr.t

    let of_versioned = Sc_rollup_inbox_repr.of_versioned

    let to_versioned = Sc_rollup_inbox_repr.to_versioned

    let get ctxt =
      let open Lwt_result_syntax in
      let* versioned = get ctxt in
      return (of_versioned versioned)

    let find ctxt =
      let open Lwt_result_syntax in
      let* versioned = find ctxt in
      return (Option.map of_versioned versioned)

    let init ctxt value = init ctxt (to_versioned value)

    let update ctxt value = update ctxt (to_versioned value)

    let add ctxt value =
      let versioned = to_versioned value in
      add ctxt versioned

    let add_or_remove ctxt value =
      add_or_remove ctxt (Option.map to_versioned value)
  end

  module Last_cemented_commitment =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["last_cemented_commitment"]
      end)
      (struct
        type t = Sc_rollup_commitment_repr.Hash.t

        let encoding = Sc_rollup_commitment_repr.Hash.encoding
      end)

  module Staker_index_counter =
    Make_single_data_storage (Registered) (Indexed_context.Raw_context)
      (struct
        let name = ["staker_index_counter"]
      end)
      (Sc_rollup_staker_index_repr)

  module Staker_index =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["staker_index"]
         end))
         (Public_key_hash_index)
      (Sc_rollup_staker_index_repr)

  module Stakers =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["stakers"]
         end))
         (Make_index (Sc_rollup_staker_index_repr.Index))
      (Raw_level_repr)

  module Commitments_versioned =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitments"]
         end))
         (Make_index (Sc_rollup_commitment_repr.Hash))
      (struct
        type t = Sc_rollup_commitment_repr.versioned

        let encoding = Sc_rollup_commitment_repr.versioned_encoding
      end)

  module Commitments = struct
    include Commitments_versioned
    include Make_versioned (Sc_rollup_commitment_repr) (Commitments_versioned)
  end

  module Commitment_indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitment_index"]
         end))
         (Make_index (Sc_rollup_commitment_repr.Hash))

  module Commitment_stakers =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitments_stakers"]
         end))
         (Make_index (Sc_rollup_commitment_repr.Hash))
      (struct
        type t = Sc_rollup_staker_index_repr.t list

        let encoding = Data_encoding.list Sc_rollup_staker_index_repr.encoding
      end)

  module Commitments_per_inbox_level =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitments_per_inbox_level"]
         end))
         (Make_index (Raw_level_repr.Index))
      (struct
        type t = Sc_rollup_commitment_repr.Hash.t list

        let encoding =
          Data_encoding.list Sc_rollup_commitment_repr.Hash.encoding
      end)

  module Commitment_first_publication_level =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitment_first_publication_level"]
         end))
         (Make_index (Raw_level_repr.Index))
      (Raw_level_repr)

  module Commitment_added =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["commitment_added"]
         end))
         (Make_index (Sc_rollup_commitment_repr.Hash))
      (Raw_level_repr)

  module Game_info_versioned =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["refutation_game_info"]
         end))
         (Make_index (Sc_rollup_game_repr.Index))
      (struct
        type t = Sc_rollup_game_repr.versioned

        let encoding = Sc_rollup_game_repr.versioned_encoding
      end)

  module Game_info = struct
    include Game_info_versioned
    include Make_versioned (Sc_rollup_game_repr) (Game_info_versioned)
  end

  module Games_per_staker =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["game"]
         end))
         (Public_key_hash_index)

  module Game =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Games_per_staker.Raw_context)
         (struct
           let name = ["opponents"]
         end))
         (Public_key_hash_index)
      (struct
        type t = Sc_rollup_game_repr.Index.t

        let encoding = Sc_rollup_game_repr.Index.encoding
      end)

  module Game_timeout =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["game_timeout"]
         end))
         (Make_index (Sc_rollup_game_repr.Index))
      (struct
        type t = Sc_rollup_game_repr.timeout

        let encoding = Sc_rollup_game_repr.timeout_encoding
      end)

  (** An index used for a SCORU's outbox levels. An outbox level is mapped to
     the index through: [outbox_level % sc_rollup_max_active_outbox_levels].
     That way we keep a limited number of entries. The current value of an
     entry contains the most recently added level that maps to the index. *)
  module Level_index = struct
    type t = int32

    let rpc_arg =
      let construct = Int32.to_string in
      let destruct hash =
        Int32.of_string_opt hash
        |> Result.of_option ~error:"Cannot parse level index"
      in
      RPC_arg.make
        ~descr:"The level index for applied outbox message records"
        ~name:"level_index"
        ~construct
        ~destruct
        ()

    let encoding =
      Data_encoding.def
        "level_index"
        ~title:"Level index"
        ~description:"The level index for applied outbox message records"
        Data_encoding.int32

    let compare = Compare.Int32.compare

    let path_length = 1

    let to_path c l = Int32.to_string c :: l

    let of_path = function [c] -> Int32.of_string_opt c | _ -> None
  end

  module Level_index_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["level_index"]
         end))
         (Make_index (Level_index))

  module Bitset_and_level = struct
    type t = Raw_level_repr.t * Bitset.t

    let encoding =
      Data_encoding.(
        obj2
          (req "level" Raw_level_repr.encoding)
          (req "bitset" Bitset.encoding))
  end

  module Applied_outbox_messages =
    Level_index_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["applied_outbox_messages"]
      end)
      (Bitset_and_level)

  module Whitelist =
    Make_carbonated_data_set_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["whitelist"]
         end))
         (Public_key_hash_index)

  module Whitelist_paid_storage_space =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["whitelist_paid_bytes"]
      end)
      (Encoding.Z)

  module Whitelist_used_storage_space =
    Indexed_context.Make_map
      (Registered)
      (struct
        let name = ["whitelist_use_bytes"]
      end)
      (Encoding.Z)

  module Last_whitelist_update =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["last_whitelist_update"]
      end)
      (struct
        type t = Sc_rollup_whitelist_repr.last_whitelist_update

        let encoding = Sc_rollup_whitelist_repr.last_whitelist_update_encoding
      end)
end

module Dal = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["dal"]
      end)

  module Slot = struct
    module Level_context =
      Make_indexed_subcontext
        (Make_subcontext (Registered) (Raw_context)
           (struct
             let name = ["level"]
           end))
           (Make_index (Raw_level_repr.Index))

    module Headers =
      Level_context.Make_map
        (Registered)
        (struct
          let name = ["slot_headers"]
        end)
        (struct
          type t = (Dal_slot_repr.Header.t * Contract_repr.t) list

          let encoding =
            let open Data_encoding in
            list
              (obj2
                 (req "slot_header" Dal_slot_repr.Header.encoding)
                 (req "publisher" Contract_repr.encoding))
        end)

    module History =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["slot_headers_history"]
        end)
        (struct
          type t = Dal_slot_repr.History.t

          let encoding = Dal_slot_repr.History.encoding
        end)

    module LevelHistories =
      Make_single_data_storage (Registered) (Raw_context)
        (struct
          let name = ["slot_headers_successive_histories_of_level"]
        end)
        (struct
          type t =
            (Dal_slot_repr.History.Pointer_hash.t * Dal_slot_repr.History.t)
            list

          let encoding =
            let open Data_encoding in
            let module H = Dal_slot_repr.History in
            list
              (obj2
                 (req "cell_hash" H.Pointer_hash.encoding)
                 (req "cell" H.encoding))
        end)
  end

  module Denounced_delegates =
    Make_indexed_data_storage
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["dal_denounced_delegates"]
         end))
         (Public_key_hash_index)
      (struct
        type t = unit

        let encoding = Data_encoding.empty
      end)

  module Past_parameters =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["past_parameters"]
      end)
      (struct
        type t = Constants_parametric_repr.past_dal_parameters list

        let encoding =
          Data_encoding.list
            Constants_parametric_repr.past_dal_parameters_encoding
      end)
end

module Zk_rollup = struct
  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["zk_rollup"]
         end))
         (Make_index (Zk_rollup_repr.Index))

  module Account :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t
       and type key = Zk_rollup_repr.t
       and type value = Zk_rollup_account_repr.t =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["account"]
      end)
      (Zk_rollup_account_repr)

  module Pending_list =
    Indexed_context.Make_carbonated_map
      (Registered)
      (struct
        let name = ["pending_list"]
      end)
      (struct
        type t = Zk_rollup_repr.pending_list

        let encoding = Zk_rollup_repr.pending_list_encoding
      end)

  module Pending_operation :
    Non_iterable_indexed_carbonated_data_storage
      with type t := Raw_context.t * Zk_rollup_repr.t
       and type key = int64
       and type value = Zk_rollup_operation_repr.t * Ticket_hash_repr.t option =
    Make_indexed_carbonated_data_storage
      (Make_subcontext (Registered) (Indexed_context.Raw_context)
         (struct
           let name = ["pending_operations"]
         end))
         (Make_index (struct
           type t = int64

           let rpc_arg =
             let construct = Int64.to_string in
             let destruct hash =
               Int64.of_string_opt hash
               |> Result.of_option
                    ~error:"Cannot parse pending operation position"
             in
             RPC_arg.make
               ~descr:
                 "The position of an operation in a pending operations list"
               ~name:"zkru_pending_op_position"
               ~construct
               ~destruct
               ()

           let encoding =
             Data_encoding.def
               "zkru_pending_op_position"
               ~title:"Zkru pending operation position"
               ~description:
                 "The position of an operation in a pending operations list"
               Data_encoding.Compact.(make ~tag_size:`Uint8 int64)

           let compare = Compare.Int64.compare

           let path_length = 1

           let to_path c l = Int64.to_string c :: l

           let of_path = function [c] -> Int64.of_string_opt c | _ -> None
         end))
      (struct
        type t = Zk_rollup_operation_repr.t * Ticket_hash_repr.t option

        let encoding =
          Data_encoding.(
            tup2
              Zk_rollup_operation_repr.encoding
              (option Ticket_hash_repr.encoding))
      end)
end

module Legacy = struct
  module Grand_parent_branch =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["grand_parent_branch"]
      end)
      (Tenderbake.Branch)
end
