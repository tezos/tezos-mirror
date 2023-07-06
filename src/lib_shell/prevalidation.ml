(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Shell_operation

module type CHAIN_STORE = sig
  type chain_store

  val context :
    chain_store ->
    Store.Block.t ->
    Tezos_protocol_environment.Context.t tzresult Lwt.t

  val chain_id : chain_store -> Chain_id.t
end

module type T = sig
  type protocol_operation

  type config

  val default_config : config

  val config_encoding : config Data_encoding.t

  type chain_store

  type t

  val create :
    chain_store ->
    head:Store.Block.t ->
    timestamp:Time.Protocol.t ->
    t tzresult Lwt.t

  val flush :
    chain_store ->
    head:Store.Block.t ->
    timestamp:Time.Protocol.t ->
    t ->
    t tzresult Lwt.t

  val pre_filter :
    t ->
    config ->
    protocol_operation Shell_operation.operation ->
    [ `Passed_prefilter of Prevalidator_pending_operations.priority
    | Prevalidator_classification.error_classification ]
    Lwt.t

  type replacements =
    (Operation_hash.t * Prevalidator_classification.error_classification) list

  type add_result =
    t
    * protocol_operation operation
    * Prevalidator_classification.classification
    * replacements

  val add_operation :
    t -> config -> protocol_operation operation -> add_result Lwt.t

  val remove_operation : t -> Operation_hash.t -> t

  module Internal_for_tests : sig
    val get_mempool_operations : t -> protocol_operation Operation_hash.Map.t

    type mempool

    val set_mempool : t -> mempool -> t

    type bounding_state

    val get_bounding_state : t -> bounding_state

    val set_bounding_state : t -> bounding_state -> t
  end
end

module MakeAbstract
    (Chain_store : CHAIN_STORE)
    (Filter : Shell_plugin.FILTER)
    (Bounding : Prevalidator_bounding.T
                  with type protocol_operation = Filter.Proto.operation) :
  T
    with type protocol_operation = Filter.Proto.operation
     and type chain_store = Chain_store.chain_store
     and type Internal_for_tests.mempool = Filter.Proto.Mempool.t
     and type Internal_for_tests.bounding_state = Bounding.state = struct
  module Proto = Filter.Proto

  type protocol_operation = Proto.operation

  type config = Filter.Mempool.config * Prevalidator_bounding.config

  let default_config =
    (Filter.Mempool.default_config, Prevalidator_bounding.default_config)

  let config_encoding =
    Data_encoding.merge_objs
      Filter.Mempool.config_encoding
      Prevalidator_bounding.config_encoding

  type chain_store = Chain_store.chain_store

  type operation = protocol_operation Shell_operation.operation

  type t = {
    validation_info : Proto.Mempool.validation_info;
        (** Static information needed by [Proto.Mempool.add_operation]. *)
    mempool : Proto.Mempool.t;
        (** Protocol representation of currently valid operations. *)
    bounding_state : Bounding.state;
        (** Representation of currently valid operations used to enforce
            mempool bounds. *)
    filter_info : Filter.Mempool.filter_info;
        (** Static information needed by [Filter.Mempool.pre_filter]. *)
    conflict_map : Filter.Mempool.Conflict_map.t;
        (** State needed by
            [Filter.Mempool.Conflict_map.fee_needed_to_replace_by_fee] in
            order to provide the [needed_fee_in_mutez] field of the
            [Operation_conflict] error (see the [translate_proto_add_result]
            function below). *)
  }

  let create_aux ?old_state chain_store head timestamp =
    let open Lwt_result_syntax in
    let* context = Chain_store.context chain_store head in
    let head_hash = Store.Block.hash head in
    let*! context =
      Block_validation.update_testchain_status
        context
        ~predecessor_hash:head_hash
        timestamp
    in
    let chain_id = Chain_store.chain_id chain_store in
    let head = (Store.Block.header head).shell in
    let* validation_info, mempool =
      Proto.Mempool.init context chain_id ~head_hash ~head ~cache:`Lazy
    in
    let* filter_info =
      match old_state with
      | None -> Filter.Mempool.init context ~head
      | Some old_state -> Filter.Mempool.flush old_state.filter_info ~head
    in
    let bounding_state = Bounding.empty in
    let conflict_map = Filter.Mempool.Conflict_map.empty in
    return {validation_info; mempool; bounding_state; filter_info; conflict_map}

  let create chain_store ~head ~timestamp =
    create_aux chain_store head timestamp

  let flush chain_store ~head ~timestamp old_state =
    create_aux ~old_state chain_store head timestamp

  let pre_filter state (filter_config, (_ : Prevalidator_bounding.config)) op =
    Filter.Mempool.pre_filter state.filter_info filter_config op.protocol

  type error_classification = Prevalidator_classification.error_classification

  type classification = Prevalidator_classification.classification

  type replacement = (Operation_hash.t * error_classification) option

  type replacements = (Operation_hash.t * error_classification) list

  type add_result = t * operation * classification * replacements

  let classification_of_trace trace =
    match classify_trace trace with
    | Branch -> `Branch_refused trace
    | Permanent -> `Refused trace
    | Temporary -> `Branch_delayed trace
    | Outdated -> `Outdated trace

  (* Wrapper around [Proto.Mempool.add_operation]. *)
  let proto_add_operation ~conflict_handler state op :
      (Proto.Mempool.t * Proto.Mempool.add_result) tzresult Lwt.t =
    Proto.Mempool.add_operation
      ~check_signature:(not op.signature_checked)
      ~conflict_handler
      state.validation_info
      state.mempool
      (op.hash, op.protocol)
    |> Lwt_result.map_error (function
           | Proto.Mempool.Validation_error trace -> trace
           | Add_conflict _ ->
               (* This cannot happen because we provide a [conflict_handler] to
                  [Proto.Mempool.add_operation]. See documentation in
                  [lib_protocol_environment/sigs/v<num>/updater.mli]
                  with [num >= 7]. *)
               assert false)

  (* Analyse the output of [Proto.Mempool.add_operation] to extract
     the potential replaced operation or return the appropriate error. *)
  let translate_proto_add_result (proto_add_result : Proto.Mempool.add_result)
      op conflict_map filter_config : replacement tzresult =
    let open Result in
    let open Validation_errors in
    match proto_add_result with
    | Added -> return_none
    | Replaced {removed} ->
        let trace =
          [Operation_replacement {old_hash = removed; new_hash = op.hash}]
        in
        return_some (removed, classification_of_trace trace)
    | Unchanged ->
        (* There was an operation conflict and [op] lost to the
           pre-existing operation. The error should indicate the fee
           that [op] would need in order to win the conflict and replace
           the old operation, if such a fee exists; otherwise the error
           should contain [None]. *)
        let needed_fee_in_mutez =
          Filter.Mempool.Conflict_map.fee_needed_to_replace_by_fee
            filter_config
            ~candidate_op:op.protocol
            ~conflict_map
        in
        error [Operation_conflict {new_hash = op.hash; needed_fee_in_mutez}]

  let update_bounding_state bounding_state bounding_config op ~proto_replacement
      =
    let open Result_syntax in
    let bounding_state =
      match proto_replacement with
      | None -> bounding_state
      | Some (replaced, _) -> Bounding.remove_operation bounding_state replaced
    in
    let* bounding_state, removed_operation_hashes =
      Result.map_error
        (fun op_to_overtake ->
          let needed_fee_in_mutez =
            Option.bind op_to_overtake (fun op_to_overtake ->
                Filter.Mempool.fee_needed_to_overtake
                  ~op_to_overtake:op_to_overtake.protocol
                  ~candidate_op:op.protocol)
          in
          [
            Validation_errors.Rejected_by_full_mempool
              {hash = op.hash; needed_fee_in_mutez};
          ])
        (Bounding.add_operation bounding_state bounding_config op)
    in
    let bounding_replacements =
      List.map
        (fun removed ->
          let err = [Validation_errors.Removed_from_full_mempool removed] in
          (removed, classification_of_trace err))
        removed_operation_hashes
    in
    return (bounding_state, bounding_replacements)

  let update_conflict_map conflict_map ~mempool_before op replacements =
    (* [mempool_before] is the protocol's mempool representation
       **before calling [Proto.Mempool.add_operation]**, so that it
       still contains the replaced operations. Indeed, it is used to
       retrieve these operations from their hash. *)
    let replacements =
      if List.is_empty replacements then []
        (* No need to call [Proto.Mempool.operations] when the list is empty. *)
      else
        let ops = Proto.Mempool.operations mempool_before in
        List.filter_map
          (fun (oph, (_ : error_classification)) ->
            (* This should always return [Some _]. *)
            Operation_hash.Map.find oph ops)
          replacements
    in
    Filter.Mempool.Conflict_map.update
      conflict_map
      ~new_operation:op.protocol
      ~replacements

  (* Implements [add_operation] but inside the [tzresult] monad. *)
  let add_operation_result state (filter_config, bounding_config) op =
    let open Lwt_result_syntax in
    let conflict_handler = Filter.Mempool.conflict_handler filter_config in
    let* mempool, proto_add_result =
      proto_add_operation ~conflict_handler state op
    in
    (* The operation might still be rejected because of a conflict
       with a previously validated operation, or if the mempool is
       full and the operation does not have enough fees. Nevertheless,
       the successful call to [Proto.Mempool.add_operation] guarantees
       that the operation is individually valid, in particular its
       signature is correct. We record this so that any future
       signature check can be skipped. *)
    let valid_op = record_successful_signature_check op in
    let res =
      catch_e @@ fun () ->
      let open Result_syntax in
      let* proto_replacement =
        translate_proto_add_result
          proto_add_result
          op
          state.conflict_map
          filter_config
      in
      let* bounding_state, bounding_replacements =
        update_bounding_state
          state.bounding_state
          bounding_config
          op
          ~proto_replacement
      in
      let mempool =
        List.fold_left
          (fun mempool (replaced_oph, _) ->
            Proto.Mempool.remove_operation mempool replaced_oph)
          mempool
          bounding_replacements
      in
      let all_replacements =
        match proto_replacement with
        | None -> bounding_replacements
        | Some proto_replacement -> proto_replacement :: bounding_replacements
      in
      let conflict_map =
        update_conflict_map
          state.conflict_map
          ~mempool_before:state.mempool
          op
          all_replacements
      in
      let state = {state with mempool; bounding_state; conflict_map} in
      return (state, valid_op, `Validated, all_replacements)
    in
    match res with
    | Ok add_result -> return add_result
    | Error trace ->
        (* When [res] is an error, we convert it to an [add_result]
           here (instead of letting [add_operation] do it below) so
           that we can return the updated [valid_op]. *)
        return (state, valid_op, classification_of_trace trace, [])

  let add_operation state config op : add_result Lwt.t =
    let open Lwt_syntax in
    let* res = protect (fun () -> add_operation_result state config op) in
    match res with
    | Ok add_result -> return add_result
    | Error trace -> return (state, op, classification_of_trace trace, [])

  let remove_operation state oph =
    let mempool = Proto.Mempool.remove_operation state.mempool oph in
    let bounding_state = Bounding.remove_operation state.bounding_state oph in
    {state with mempool; bounding_state}

  module Internal_for_tests = struct
    let get_mempool_operations {mempool; _} = Proto.Mempool.operations mempool

    type mempool = Proto.Mempool.t

    let set_mempool state mempool = {state with mempool}

    type bounding_state = Bounding.state

    let get_bounding_state {bounding_state; _} = bounding_state

    let set_bounding_state state bounding_state = {state with bounding_state}
  end
end

module Production_chain_store :
  CHAIN_STORE with type chain_store = Store.chain_store = struct
  type chain_store = Store.chain_store

  let context = Store.Block.context

  let chain_id = Store.Chain.chain_id
end

module Make (Filter : Shell_plugin.FILTER) :
  T
    with type protocol_operation = Filter.Proto.operation
     and type chain_store = Store.chain_store =
  MakeAbstract (Production_chain_store) (Filter)
    (Prevalidator_bounding.Make (Filter.Proto))

module Internal_for_tests = struct
  module type CHAIN_STORE = CHAIN_STORE

  module Make = MakeAbstract
end
