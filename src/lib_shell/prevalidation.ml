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

  type validation_state

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
     and type validation_state = Filter.Proto.validation_state
     and type chain_store = Chain_store.chain_store
     and type Internal_for_tests.mempool = Filter.Proto.Mempool.t
     and type Internal_for_tests.bounding_state = Bounding.state = struct
  module Proto = Filter.Proto

  type protocol_operation = Proto.operation

  type validation_state = Proto.validation_state

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
    mempool : Proto.Mempool.t;
    bounding_state : Bounding.state;
    filter_info : Filter.Mempool.filter_info;
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
    return
      {validation_info; mempool; bounding_state = Bounding.empty; filter_info}

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

  let translate_proto_add_result (proto_add_result : Proto.Mempool.add_result)
      op : replacement tzresult =
    let open Result in
    let open Validation_errors in
    match proto_add_result with
    | Added -> return_none
    | Replaced {removed} ->
        let trace =
          [Operation_replacement {old_hash = removed; new_hash = op.hash}]
        in
        return_some (removed, `Outdated trace)
    | Unchanged -> error [Operation_conflict {new_hash = op.hash}]

  let bounding_add_operation bounding_state bounding_config op =
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

  (* Analyze the output of [Proto.Mempool.add_operation] to handle a
     potential operation conflict. Then use the [Bounding] module to
     ensure that the mempool remains bounded.

     If successful, return the updated [mempool] and [bounding_state],
     as well as any operation [replacements] caused by either the
     protocol mempool or the [Bounding] module.

     Note that the [mempool] argument, as part of the output of
     [Proto.Mempool.add_operation], already contains the new operation
     (if it has been accepted). So the only update it may need is the
     removal of any operations replaced during [Bounding.add]. *)
  let check_conflict_and_bound (mempool, proto_add_result) bounding_state
      bounding_config op :
      (Proto.Mempool.t * Bounding.state * replacements) tzresult =
    let open Result_syntax in
    let* proto_replacement = translate_proto_add_result proto_add_result op in
    let bounding_state =
      match proto_replacement with
      | None -> bounding_state
      | Some (replaced, _) -> Bounding.remove_operation bounding_state replaced
    in
    let* bounding_state, removed_by_bounding =
      bounding_add_operation bounding_state bounding_config op
    in
    let mempool =
      List.fold_left Proto.Mempool.remove_operation mempool removed_by_bounding
    in
    let replacements =
      Option.to_list proto_replacement
      @ List.map
          (fun removed ->
            let err = [Validation_errors.Removed_from_full_mempool removed] in
            (removed, classification_of_trace err))
          removed_by_bounding
    in
    return (mempool, bounding_state, replacements)

  let add_operation_result state (filter_config, bounding_config) op :
      add_result tzresult Lwt.t =
    let open Lwt_result_syntax in
    let conflict_handler = Filter.Mempool.conflict_handler filter_config in
    let* proto_output = proto_add_operation ~conflict_handler state op in
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
      check_conflict_and_bound
        proto_output
        state.bounding_state
        bounding_config
        valid_op
    in
    match res with
    | Ok (mempool, bounding_state, replacement) ->
        let state = {state with mempool; bounding_state} in
        return (state, valid_op, `Prechecked, replacement)
    | Error trace ->
        (* We convert any error from [check_conflict_and_bound] into an
           [add_result] here, rather than let [add_operation] below do
           the same, so that we can return the updated [valid_op]. *)
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
     and type validation_state = Filter.Proto.validation_state
     and type chain_store = Store.chain_store =
  MakeAbstract (Production_chain_store) (Filter)
    (Prevalidator_bounding.Make (Filter.Proto))

module Internal_for_tests = struct
  module type CHAIN_STORE = CHAIN_STORE

  module Make = MakeAbstract
end
