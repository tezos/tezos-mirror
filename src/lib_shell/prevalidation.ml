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

  type filter_state

  type filter_config

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
    filter_config ->
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
    t -> filter_config -> protocol_operation operation -> add_result Lwt.t

  val remove_operation : t -> Operation_hash.t -> t

  module Internal_for_tests : sig
    val get_mempool_operations : t -> protocol_operation Operation_hash.Map.t

    val get_filter_state : t -> filter_state

    type mempool

    val set_mempool : t -> mempool -> t
  end
end

module MakeAbstract (Chain_store : CHAIN_STORE) (Filter : Shell_plugin.FILTER) :
  T
    with type protocol_operation = Filter.Proto.operation
     and type validation_state = Filter.Proto.validation_state
     and type filter_state = Filter.Mempool.state
     and type filter_config = Filter.Mempool.config
     and type chain_store = Chain_store.chain_store
     and type Internal_for_tests.mempool = Filter.Proto.Mempool.t = struct
  module Proto = Filter.Proto

  type protocol_operation = Proto.operation

  type validation_state = Proto.validation_state

  type filter_state = Filter.Mempool.state

  type filter_config = Filter.Mempool.config

  type chain_store = Chain_store.chain_store

  type operation = protocol_operation Shell_operation.operation

  type create_aux_t = {
    validation_info : Proto.Mempool.validation_info;
    mempool : Proto.Mempool.t;
    head : Block_header.shell_header;
    context : Tezos_protocol_environment.Context.t;
  }

  let create_aux chain_store head timestamp =
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
    return {validation_info; mempool; head; context}

  type t = {
    validation_info : Proto.Mempool.validation_info;
    mempool : Proto.Mempool.t;
    filter_state : Filter.Mempool.state;
  }

  let create chain_store ~head ~timestamp =
    let open Lwt_result_syntax in
    let* {validation_info; mempool; head; context} =
      create_aux chain_store head timestamp
    in
    let* filter_state = Filter.Mempool.init context ~head in
    return {validation_info; mempool; filter_state}

  let flush chain_store ~head ~timestamp old_state =
    let open Lwt_result_syntax in
    let* {validation_info; mempool; head; context = _} =
      create_aux chain_store head timestamp
    in
    let* filter_state = Filter.Mempool.flush old_state.filter_state ~head in
    return {validation_info; mempool; filter_state}

  let pre_filter state filter_config op =
    Filter.Mempool.pre_filter
      ~filter_state:state.filter_state
      filter_config
      op.protocol

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
      op : (replacement, error_classification) result =
    let open Result in
    let open Validation_errors in
    match proto_add_result with
    | Added -> return_none
    | Replaced {removed} ->
        let trace =
          [Operation_replacement {old_hash = removed; new_hash = op.hash}]
        in
        return_some (removed, `Outdated trace)
    | Unchanged ->
        error
          (classification_of_trace [Operation_conflict {new_hash = op.hash}])

  (** Call [Filter.Mempool.add_operation_and_enforce_mempool_bound],
      which ensures that the number of manager operations in the
      mempool is bounded as specified in [filter_config].

      The [state] argument is the prevalidation state (which has not
      been modified yet). The [mempool] and [proto_add_result] are the
      results of the protocol's [add_operation].

      Maintaining this bound may require the removal of an operation
      when the mempool was already full. In this case, this operation,
      called [full_mempool_replacement], must also be removed from the
      protocol's abstract [mempool].

      Return the updated [state] (containing the updated protocol
      [mempool]) and [filter_state], and the final [replacements], which
      may have been mandated either by the protocol's [add_operation]
      or by [Filter.Mempool.add_operation_and_enforce_mempool_bound]
      (but not both: if the protocol already causes a replacement, then
      the mempool is no longer full so there cannot be a
      [full_mempool_replacement]. *)
  let enforce_mempool_bound_and_update_states state filter_config
      (mempool, proto_add_result) op :
      (t * replacements, error_classification) result Lwt.t =
    let open Lwt_result_syntax in
    let*? proto_replacement = translate_proto_add_result proto_add_result op in
    let* filter_state, full_mempool_replacement =
      Filter.Mempool.add_operation_and_enforce_mempool_bound
        ?replace:(Option.map fst proto_replacement)
        filter_config
        state.filter_state
        (op.hash, op.protocol)
    in
    let mempool =
      match full_mempool_replacement with
      | `No_replace -> mempool
      | `Replace (replace_oph, _) ->
          Proto.Mempool.remove_operation mempool replace_oph
    in
    let replacements =
      match (proto_replacement, full_mempool_replacement) with
      | _, `No_replace -> Option.to_list proto_replacement
      | None, `Replace repl -> [repl]
      | Some _, `Replace _ ->
          (* If there is a [proto_replacement], it gets removed from the
             mempool before adding [op] so the mempool cannot be full. *)
          assert false
    in
    return ({state with mempool; filter_state}, replacements)

  let add_operation_result state filter_config op :
      (t * operation * classification * replacements) tzresult Lwt.t =
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
    let op = record_successful_signature_check op in
    let*! res =
      enforce_mempool_bound_and_update_states
        state
        filter_config
        proto_output
        op
    in
    match res with
    | Ok (state, replacement) -> return (state, op, `Prechecked, replacement)
    | Error err_class -> return (state, op, (err_class :> classification), [])

  let add_operation state filter_config op : add_result Lwt.t =
    let open Lwt_syntax in
    let* res =
      protect (fun () -> add_operation_result state filter_config op)
    in
    match res with
    | Ok add_result -> return add_result
    | Error trace -> return (state, op, classification_of_trace trace, [])

  let remove_operation state oph =
    let mempool = Proto.Mempool.remove_operation state.mempool oph in
    let filter_state =
      Filter.Mempool.remove ~filter_state:state.filter_state oph
    in
    {state with mempool; filter_state}

  module Internal_for_tests = struct
    let get_mempool_operations {mempool; _} = Proto.Mempool.operations mempool

    let get_filter_state {filter_state; _} = filter_state

    type mempool = Proto.Mempool.t

    let set_mempool state mempool = {state with mempool}
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
     and type filter_state = Filter.Mempool.state
     and type filter_config = Filter.Mempool.config
     and type chain_store = Store.chain_store =
  MakeAbstract (Production_chain_store) (Filter)

module Internal_for_tests = struct
  module type CHAIN_STORE = CHAIN_STORE

  module Make = MakeAbstract
end
