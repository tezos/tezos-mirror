(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Baking_errors

type incremental = {
  predecessor : Baking_state_types.block_info;
  context : Tezos_protocol_environment.Context.t;
  state : Protocol.validation_state * Protocol.application_state option;
  rev_operations : Operation.packed list;
  header : Tezos_base.Block_header.shell_header;
}

let load_context ~data_dir =
  let open Lwt_result_syntax in
  protect (fun () ->
      let* index = Context_ops.init ~kind:`Disk ~readonly:true ~data_dir () in
      return (Abstract_context_index.abstract index))

let check_context_consistency (abstract_index : Abstract_context_index.t)
    context_hash =
  let open Lwt_result_syntax in
  protect (fun () ->
      (* Hypothesis : the version key exists *)
      let version_key = ["version"] in
      let*! context_opt = abstract_index.checkout_fun context_hash in
      match context_opt with
      | None -> tzfail Failed_to_checkout_context
      | Some context -> (
          let*! result = Context_ops.mem context version_key in
          match result with
          | true -> return_unit
          | false -> tzfail Invalid_context))

let begin_construction ~timestamp ~protocol_data ~force_apply
    ~pred_resulting_context_hash (abstract_index : Abstract_context_index.t)
    pred_block chain_id =
  let open Lwt_result_syntax in
  protect (fun () ->
      let {Baking_state_types.shell = pred_shell; hash = pred_hash; _} =
        pred_block
      in
      let*! context_opt =
        abstract_index.checkout_fun pred_resulting_context_hash
      in
      match context_opt with
      | None -> tzfail Failed_to_checkout_context
      | Some context ->
          let header : Tezos_base.Block_header.shell_header =
            Tezos_base.Block_header.
              {
                predecessor = pred_hash;
                proto_level = pred_shell.proto_level;
                validation_passes = 0;
                fitness = pred_shell.fitness;
                timestamp;
                level = pred_shell.level;
                context = Context_hash.zero (* fake context hash *);
                operations_hash =
                  Operation_list_list_hash.zero (* fake op hash *);
              }
          in
          let mode =
            Lifted_protocol.Construction
              {
                predecessor_hash = pred_hash;
                timestamp;
                block_header_data = protocol_data;
              }
          in
          let* validation_state =
            Lifted_protocol.begin_validation
              context
              chain_id
              mode
              ~predecessor:pred_shell
              ~cache:`Lazy
          in
          let* application_state =
            if force_apply then
              let* application_state =
                Lifted_protocol.begin_application
                  context
                  chain_id
                  mode
                  ~predecessor:pred_shell
                  ~cache:`Lazy
              in
              return_some application_state
            else return_none
          in
          let state = (validation_state, application_state) in
          return
            {
              predecessor = pred_block;
              context;
              state;
              rev_operations = [];
              header;
            })

let ( let** ) x k =
  let open Lwt_result_syntax in
  let*! x in
  let*? x = Environment.wrap_tzresult x in
  k x

let add_operation st (op : Operation.packed) =
  let open Lwt_result_syntax in
  protect (fun () ->
      let validation_state, application_state = st.state in
      let oph = Operation.hash_packed op in
      let** validation_state =
        Protocol.validate_operation
          ~check_signature:false
            (* We assume that the operation has already been validated in the
               node, therefore the signature has already been checked, but we
               still need to validate it again because the context may be
               different. *)
          validation_state
          oph
          op
      in
      let** application_state, receipt =
        match application_state with
        | Some application_state ->
            let* application_state, receipt =
              Protocol.apply_operation application_state oph op
            in
            return (Some application_state, Some receipt)
        | None -> return (None, None)
      in
      let state = (validation_state, application_state) in
      return ({st with state; rev_operations = op :: st.rev_operations}, receipt))

let finalize_construction inc =
  let open Lwt_result_syntax in
  protect (fun () ->
      let validation_state, application_state = inc.state in
      let** () = Protocol.finalize_validation validation_state in
      let** result =
        match application_state with
        | Some application_state ->
            let* result =
              Protocol.finalize_application application_state (Some inc.header)
            in
            return_some result
        | None -> return_none
      in
      return result)
