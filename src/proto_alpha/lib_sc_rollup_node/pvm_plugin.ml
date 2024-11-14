(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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
open Alpha_context
open Context_wrapper

let context = Pvm.context

let get_tick kind state =
  let open Lwt_syntax in
  let open (val Pvm.of_kind kind) in
  let+ tick = get_tick (Ctxt_wrapper.of_node_pvmstate state) in
  Sc_rollup.Tick.to_z tick

let state_hash kind state =
  let open Lwt_syntax in
  let open (val Pvm.of_kind kind) in
  let+ hash = state_hash (Ctxt_wrapper.of_node_pvmstate state) in
  Sc_rollup_proto_types.State_hash.to_octez hash

let initial_state kind =
  let open Lwt_syntax in
  let open (val Pvm.of_kind kind) in
  let+ state = initial_state ~empty:(State.empty ()) in
  Ctxt_wrapper.to_node_pvmstate state

let parse_boot_sector kind =
  let module PVM = (val Pvm.of_kind kind) in
  PVM.parse_boot_sector

let install_boot_sector kind state boot_sector =
  let open Lwt_syntax in
  let open (val Pvm.of_kind kind) in
  let+ state =
    install_boot_sector (Ctxt_wrapper.of_node_pvmstate state) boot_sector
  in
  Ctxt_wrapper.to_node_pvmstate state

let get_current_level kind state =
  let open Lwt_option_syntax in
  let open (val Pvm.of_kind kind) in
  let+ current_level =
    get_current_level (Ctxt_wrapper.of_node_pvmstate state)
  in
  Raw_level.to_int32 current_level

let get_status (node_ctxt : _ Node_context.t) state =
  let open Lwt_result_syntax in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let state = PVM.Ctxt_wrapper.of_node_pvmstate state in
  let*! current_level = PVM.get_current_level state in
  let* constants =
    match current_level with
    | None -> return (Reference.get node_ctxt.current_protocol).constants
    | Some level ->
        Protocol_plugins.get_constants_of_level
          node_ctxt
          (Raw_level.to_int32 level)
  in
  let is_reveal_enabled =
    constants.sc_rollup.reveal_activation_level
    |> WithExceptions.Option.get ~loc:__LOC__
    |> Sc_rollup_proto_types.Constants.reveal_activation_level_of_octez
    |> Protocol.Alpha_context.Sc_rollup.is_reveal_enabled_predicate
  in
  let*! status = PVM.get_status ~is_reveal_enabled state in
  return (PVM.string_of_status status)

module Fueled = Fueled_pvm

let start_of_level_serialized =
  let open Sc_rollup_inbox_message_repr in
  unsafe_to_string start_of_level_serialized

let end_of_level_serialized =
  let open Sc_rollup_inbox_message_repr in
  unsafe_to_string end_of_level_serialized

let protocol_migration_serialized =
  let open Sc_rollup_inbox_message_repr in
  Some (unsafe_to_string Raw_context.protocol_migration_serialized_message)

let info_per_level_serialized ~predecessor ~predecessor_timestamp =
  let open Sc_rollup_inbox_message_repr in
  unsafe_to_string
    (info_per_level_serialized ~predecessor ~predecessor_timestamp)

let find_whitelist_update_output_index node_ctxt state ~outbox_level =
  let open Lwt_syntax in
  let outbox_level = Raw_level.of_int32_exn outbox_level in
  let open (val Pvm.of_kind node_ctxt.Node_context.kind) in
  let* outbox = get_outbox outbox_level (Ctxt_wrapper.of_node_pvmstate state) in
  let rec aux i = function
    | [] -> None
    | Sc_rollup.{message = Whitelist_update _; _} :: _rest -> Some i
    | _ :: rest -> aux (i - 1) rest
  in
  (* looking for the last whitelist update produced by the kernel,
     list is reverted for this reason. *)
  aux (List.length outbox - 1) (List.rev outbox) |> return

let outbox_transaction_summary
    (transaction : Sc_rollup.Outbox.Message.transaction) =
  Outbox_message.
    {
      destination = Contract_hash.to_b58check transaction.destination;
      entrypoint = Entrypoint.to_string transaction.entrypoint;
      parameters =
        (Michelson_v1_printer.unparse_expression
           transaction.unparsed_parameters)
          .unexpanded;
      parameters_ty = None;
    }

let outbox_typed_transaction_summary
    (transaction : Sc_rollup.Outbox.Message.typed_transaction) =
  Outbox_message.
    {
      destination = Contract_hash.to_b58check transaction.destination;
      entrypoint = Entrypoint.to_string transaction.entrypoint;
      parameters =
        (Michelson_v1_printer.unparse_expression
           transaction.unparsed_parameters)
          .unexpanded;
      parameters_ty =
        Some
          (Michelson_v1_printer.unparse_expression transaction.unparsed_ty)
            .unexpanded;
    }

let outbox_message_summary (output : Sc_rollup.output) =
  let summary =
    match output with
    | {message = Whitelist_update pkhs; _} ->
        Outbox_message.Whitelist_update pkhs
    | {message = Atomic_transaction_batch {transactions}; _} ->
        let transactions = List.map outbox_transaction_summary transactions in
        Transaction_batch transactions
    | {message = Atomic_transaction_batch_typed {transactions}; _} ->
        let transactions =
          List.map outbox_typed_transaction_summary transactions
        in
        Transaction_batch transactions
  in
  (Z.to_int output.message_index, summary)

let get_outbox_messages node_ctxt state ~outbox_level =
  let open Lwt_syntax in
  let outbox_level = Raw_level.of_int32_exn outbox_level in
  let open (val Pvm.of_kind node_ctxt.Node_context.kind) in
  let* outbox = get_outbox outbox_level (Ctxt_wrapper.of_node_pvmstate state) in
  List.rev_map outbox_message_summary outbox |> List.rev |> return

let produce_serialized_output_proof node_ctxt state ~outbox_level ~message_index
    =
  let open Lwt_result_syntax in
  let module PVM = (val Pvm.of_kind node_ctxt.Node_context.kind) in
  let state = PVM.Ctxt_wrapper.of_node_pvmstate state in
  let outbox_level = Raw_level.of_int32_exn outbox_level in
  let*! outbox = PVM.get_outbox outbox_level state in
  let output = List.nth outbox message_index in
  match output with
  | None -> invalid_arg "invalid index"
  | Some output -> (
      let*! proof =
        PVM.produce_output_proof
          (PVM.Ctxt_wrapper.of_node_context node_ctxt.context).index
          state
          output
      in
      match proof with
      | Ok proof ->
          let serialized_proof =
            Data_encoding.Binary.to_string_exn PVM.output_proof_encoding proof
          in
          return serialized_proof
      | Error err ->
          failwith
            "Error producing outbox proof (%a)"
            Environment.Error_monad.pp
            err)

module Wasm_2_0_0 = struct
  (* We can use Irmin directly here as the Wasm PVM uses only Irmin *)
  open Irmin

  let decode_durable_state enc tree =
    Wasm_2_0_0_pvm.Durable_state.Tree_encoding_runner.decode
      enc
      (of_node_pvmstate tree)

  let proof_mem_tree tree =
    Wasm_2_0_0_pvm.Wasm_2_0_0_proof_format.Tree.mem_tree (of_node_pvmstate tree)

  let proof_fold_tree ?depth tree key ~order ~init ~f =
    Wasm_2_0_0_pvm.Wasm_2_0_0_proof_format.Tree.fold
      ?depth
      (of_node_pvmstate tree)
      key
      ~order
      ~init
      ~f:(fun a b c -> f a (to_node_pvmstate b) c)
end

module Unsafe = struct
  let apply_patch (kind : Octez_smart_rollup.Kind.t) state
      (patch : Pvm_patches.unsafe_patch) =
    let open Lwt_result_syntax in
    let open (val Pvm.of_kind kind) in
    let*? patch = Unsafe_patches.of_patch patch in
    let* state =
      protect @@ fun () ->
      Unsafe_patches.apply (Ctxt_wrapper.of_node_pvmstate state) patch
      |> Lwt_result.ok
    in
    return (Ctxt_wrapper.to_node_pvmstate state)
end
