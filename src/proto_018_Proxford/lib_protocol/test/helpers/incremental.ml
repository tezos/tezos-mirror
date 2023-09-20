(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Protocol
module Proto_Nonce = Nonce (* Renamed otherwise is masked by Alpha_context *)
open Alpha_context

type t = {
  predecessor : Block.t;
  state : validation_state * application_state;
  rev_operations : Operation.packed list;
  rev_tickets : operation_receipt list;
  header : Block_header.t;
  delegate : Account.t;
  constants : Constants.Parametric.t;
}

type incremental = t

let predecessor {predecessor; _} = predecessor

let header {header; _} = header

let rev_tickets {rev_tickets; _} = rev_tickets

let validation_state {state = vs, _; _} = vs

let level st = st.header.shell.level

let alpha_ctxt {state = _, application_state; _} = application_state.ctxt

let rpc_context st =
  let fitness = (header st).shell.fitness in
  let result = Alpha_context.finalize (alpha_ctxt st) fitness in
  {
    Environment.Updater.block_hash = Block_hash.zero;
    block_header = {st.header.shell with fitness = result.fitness};
    context = result.context;
  }

let rpc_ctxt =
  new Environment.proto_rpc_context_of_directory
    rpc_context
    Plugin.RPC.rpc_services

let set_alpha_ctxt st ctxt =
  {st with state = (fst st.state, {(snd st.state) with ctxt})}

let begin_validation_and_application ctxt chain_id mode ~predecessor =
  let open Lwt_result_syntax in
  let* validation_state = begin_validation ctxt chain_id mode ~predecessor in
  let* application_state = begin_application ctxt chain_id mode ~predecessor in
  return (validation_state, application_state)

let begin_construction ?timestamp ?seed_nonce_hash ?(mempool_mode = false)
    ?(policy = Block.By_round 0) (predecessor : Block.t) =
  Block.get_next_baker ~policy predecessor
  >>=? fun (delegate, _consensus_key, round, real_timestamp) ->
  Account.find delegate >>=? fun delegate ->
  Round.of_int round |> Environment.wrap_tzresult >>?= fun payload_round ->
  let timestamp = Option.value ~default:real_timestamp timestamp in
  (match seed_nonce_hash with
  | Some _hash -> return seed_nonce_hash
  | None -> (
      Plugin.RPC.current_level ~offset:1l Block.rpc_ctxt predecessor
      >|=? function
      | {expected_commitment = true; _} -> Some (fst (Proto_Nonce.generate ()))
      | {expected_commitment = false; _} -> None))
  >>=? fun seed_nonce_hash ->
  let shell : Block_header.shell_header =
    {
      predecessor = predecessor.hash;
      proto_level = predecessor.header.shell.proto_level;
      validation_passes = predecessor.header.shell.validation_passes;
      fitness = predecessor.header.shell.fitness;
      timestamp;
      level = predecessor.header.shell.level;
      context = Context_hash.zero;
      operations_hash = Operation_list_list_hash.zero;
    }
  in
  Block.Forge.contents
    ?seed_nonce_hash
    ~payload_hash:Block_payload_hash.zero
    ~payload_round
    shell
  >>=? fun contents ->
  let mode =
    if mempool_mode then
      Partial_construction {predecessor_hash = predecessor.hash; timestamp}
    else
      let block_header_data =
        {Block_header.contents; signature = Signature.zero}
      in
      Construction
        {predecessor_hash = predecessor.hash; timestamp; block_header_data}
  in
  let header =
    {Block_header.shell; protocol_data = {contents; signature = Signature.zero}}
  in
  begin_validation_and_application
    predecessor.context
    Chain_id.zero
    mode
    ~predecessor:predecessor.header.shell
  >|= fun state ->
  Environment.wrap_tzresult state >|? fun state ->
  {
    predecessor;
    state;
    rev_operations = [];
    rev_tickets = [];
    header;
    delegate;
    constants = predecessor.constants;
  }

let detect_script_failure :
    type kind. kind Apply_results.operation_metadata -> _ =
  let rec detect_script_failure :
      type kind. kind Apply_results.contents_result_list -> _ =
    let open Apply_results in
    let open Apply_operation_result in
    let open Apply_internal_results in
    let detect_script_failure_single (type kind)
        (Manager_operation_result
           {operation_result; internal_operation_results; _} :
          kind Kind.manager Apply_results.contents_result) =
      let detect_script_failure (type kind)
          (result : (kind, _, _) operation_result) =
        match result with
        | Applied _ -> Ok ()
        | Skipped _ -> assert false
        | Backtracked (_, None) ->
            (* there must be another error for this to happen *)
            Ok ()
        | Backtracked (_, Some errs) -> Error (Environment.wrap_tztrace errs)
        | Failed (_, errs) -> Error (Environment.wrap_tztrace errs)
      in
      detect_script_failure operation_result >>? fun () ->
      List.iter_e
        (fun (Internal_operation_result (_, r)) -> detect_script_failure r)
        internal_operation_results
    in
    function
    | Single_result (Manager_operation_result _ as res) ->
        detect_script_failure_single res
    | Single_result _ -> Ok ()
    | Cons_result (res, rest) ->
        detect_script_failure_single res >>? fun () ->
        detect_script_failure rest
  in
  fun {contents} -> detect_script_failure contents

let check_operation_size ?(check_size = true) op =
  if check_size then
    let operation_size =
      Data_encoding.Binary.length
        Operation.encoding_with_legacy_attestation_name
        op
    in
    if operation_size > Constants_repr.max_operation_data_length then
      raise
        (invalid_arg
           (Format.sprintf
              "The operation size is %d: it exceeds the constant maximum size \
               %d."
              operation_size
              Constants_repr.max_operation_data_length))

let validate_operation ?expect_failure ?check_size st op =
  let open Lwt_result_syntax in
  check_operation_size ?check_size op ;
  let validation_state, application_state = st.state in
  let oph = Operation.hash_packed op in
  let*! res = validate_operation validation_state oph op in
  match (expect_failure, Environment.wrap_tzresult res) with
  | Some _, Ok _ -> failwith "Error expected while validating operation"
  | Some f, Error err ->
      let* () = f err in
      return st
  | None, Error err -> fail err
  | None, Ok validation_state ->
      return {st with state = (validation_state, application_state)}

let add_operation ?expect_failure ?expect_apply_failure ?allow_manager_failure
    ?check_size st op =
  let open Lwt_result_syntax in
  let open Apply_results in
  let* st = validate_operation ?expect_failure ?check_size st op in
  match expect_failure with
  | Some _ ->
      (* The expected failure has already been observed in
         [validate_operation]. *)
      return st
  | None -> (
      let validation_state, application_state = st.state in
      let oph = Operation.hash_packed op in
      let*! res = apply_operation application_state oph op in
      let*? application_state, metadata = Environment.wrap_tzresult res in
      let st =
        {
          st with
          state = (validation_state, application_state);
          rev_operations = op :: st.rev_operations;
          rev_tickets = metadata :: st.rev_tickets;
        }
      in
      match allow_manager_failure with
      | Some true -> return st
      | None | Some false -> (
          match (expect_apply_failure, metadata) with
          | None, No_operation_metadata -> return st
          | None, Operation_metadata result ->
              let*? () = detect_script_failure result in
              return st
          | Some _, No_operation_metadata ->
              failwith "Error expected while adding operation"
          | Some f, Operation_metadata result -> (
              match detect_script_failure result with
              | Ok _ -> failwith "Error expected while adding operation"
              | Error err ->
                  let* () = f err in
                  return st)))

let finalize_validation_and_application (validation_state, application_state)
    shell_header =
  let open Lwt_result_syntax in
  let* () = finalize_validation validation_state in
  finalize_application application_state shell_header

let finalize_block st =
  let open Lwt_result_syntax in
  let operations = List.rev st.rev_operations in
  let operations_hash =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute (List.map Operation.hash_packed operations)]
  in
  let shell_header =
    {
      st.header.shell with
      level = Int32.succ st.header.shell.level;
      operations_hash;
    }
  in
  let*! res =
    finalize_validation_and_application st.state (Some shell_header)
  in
  let*? validation_result, _ = Environment.wrap_tzresult res in
  let operations = List.rev st.rev_operations in
  let operations_hash =
    Operation_list_list_hash.compute
      [Operation_list_hash.compute (List.map Operation.hash_packed operations)]
  in
  let header =
    {
      st.header with
      shell =
        {
          st.header.shell with
          level = Int32.succ st.header.shell.level;
          operations_hash;
          fitness = validation_result.fitness;
        };
    }
  in
  let hash = Block_header.hash header in
  return
    {
      Block.hash;
      header;
      operations;
      context = validation_result.context;
      constants = st.constants;
    }

let assert_validate_operation_fails expect_failure op block =
  let open Lwt_result_syntax in
  let* i = begin_construction block in
  let* (_i : incremental) = validate_operation ~expect_failure i op in
  return_unit
