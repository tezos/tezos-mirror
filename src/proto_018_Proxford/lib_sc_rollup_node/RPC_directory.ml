(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

open Rpc_directory_helpers
open Protocol
open Context_wrapper.Irmin

module Slot_pages_map = struct
  open Protocol
  open Alpha_context
  include Map.Make (Dal.Slot_index)
end

module Block_directory = Make_sub_directory (struct
  include Sc_rollup_services.Block

  type context = Node_context.rw

  type subcontext = Node_context.ro * Block_hash.t

  let context_of_prefix node_ctxt (((), block) : prefix) =
    let open Lwt_result_syntax in
    let+ block = Block_directory_helpers.block_of_prefix node_ctxt block in
    (Node_context.readonly node_ctxt, block)
end)

module Block_helpers_directory = Make_sub_directory (struct
  include Sc_rollup_services.Block.Helpers

  (* The context needs to be accessed with write permissions because we need to
     commit on disk to generate the proofs. *)
  type context = Node_context.rw

  (* The context needs to be accessed with write permissions because we need to
     commit on disk to generate the proofs. *)
  type subcontext = Node_context.rw * Block_hash.t

  let context_of_prefix node_ctxt (((), block) : prefix) =
    let open Lwt_result_syntax in
    let+ block = Block_directory_helpers.block_of_prefix node_ctxt block in
    (node_ctxt, block)
end)

let get_state (node_ctxt : _ Node_context.t) block_hash =
  let open Lwt_result_syntax in
  let* ctxt = Node_context.checkout_context node_ctxt block_hash in
  let*! state = Context.PVMState.find ctxt in
  match state with
  | None -> failwith "No state"
  | Some state -> return (state, !(of_node_pvmstate state))

let simulate_messages (node_ctxt : Node_context.ro) block ~reveal_pages
    ~insight_requests ~log_kernel_debug_file messages =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let reveal_map =
    match reveal_pages with
    | Some pages ->
        let map =
          List.fold_left
            (fun map page ->
              let hash =
                Protocol.Sc_rollup_reveal_hash.hash_string
                  ~scheme:Blake2B
                  [page]
                |> Data_encoding.Binary.to_bytes_exn
                     Protocol.Sc_rollup_reveal_hash.encoding
              in
              Utils.Reveal_hash_map.add hash page map)
            Utils.Reveal_hash_map.empty
            pages
        in
        Some map
    | None -> None
  in
  let* level = Node_context.level_of_hash node_ctxt block in
  let* sim =
    Simulation.start_simulation
      node_ctxt
      ~reveal_map
      ?log_kernel_debug_file
      Layer1.{hash = block; level}
  in
  let* sim, num_ticks_0 = Simulation.simulate_messages sim messages in
  let* {state; inbox_level; _}, num_ticks_end = Simulation.end_simulation sim in
  let*! insights =
    List.map_p
      (function
        | Sc_rollup_services.Pvm_state_key key ->
            PVM.Mutable_state.lookup (of_node_pvmstate state) key
        | Durable_storage_key key ->
            PVM.Mutable_state.Inspect_durable_state.lookup
              (of_node_pvmstate state)
              key)
      insight_requests
  in
  let num_ticks = Z.(num_ticks_0 + num_ticks_end) in
  let level = Raw_level.of_int32_exn inbox_level in
  let*! outbox = PVM.get_outbox level !(of_node_pvmstate state) in
  let output =
    List.filter (fun Sc_rollup.{outbox_level; _} -> outbox_level = level) outbox
  in
  let*! state_hash = PVM.state_hash !(of_node_pvmstate state) in
  let* constants =
    Protocol_plugins.get_constants_of_level node_ctxt inbox_level
  in
  let is_reveal_enabled =
    constants.sc_rollup.reveal_activation_level
    |> WithExceptions.Option.get ~loc:__LOC__
    |> Sc_rollup_proto_types.Constants.reveal_activation_level_of_octez
    |> Protocol.Alpha_context.Sc_rollup.is_reveal_enabled_predicate
  in
  let*! status = PVM.get_status ~is_reveal_enabled !(of_node_pvmstate state) in
  let status = PVM.string_of_status status in
  return
    Sc_rollup_services.
      {state_hash; status; output; inbox_level; num_ticks; insights}

let () =
  Block_directory.register0 Sc_rollup_services.Block.status
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* _, state = get_state node_ctxt block in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let* constants =
    Protocol_plugins.get_constants_of_block_hash node_ctxt block
  in
  let is_reveal_enabled =
    constants.sc_rollup.reveal_activation_level
    |> WithExceptions.Option.get ~loc:__LOC__
    |> Sc_rollup_proto_types.Constants.reveal_activation_level_of_octez
    |> Protocol.Alpha_context.Sc_rollup.is_reveal_enabled_predicate
  in
  let*! status = PVM.get_status ~is_reveal_enabled state in
  return (PVM.string_of_status status)

let () =
  Block_directory.register0 Sc_rollup_services.Block.outbox
  @@ fun (node_ctxt, block) outbox_level () ->
  let open Lwt_result_syntax in
  let* _, state = get_state node_ctxt block in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let*! outbox = PVM.get_outbox outbox_level state in
  return outbox

let () =
  Block_directory.register1 Sc_rollup_services.Block.outbox_messages
  @@ fun (node_ctxt, block) outbox_level () () ->
  let open Lwt_result_syntax in
  let* _, state = get_state node_ctxt block in
  let module PVM = (val Pvm.of_kind node_ctxt.kind) in
  let*! outbox = PVM.get_outbox outbox_level state in
  return outbox

let () =
  Block_helpers_directory.register1
    Sc_rollup_services.Block.Helpers.outbox_proof_simple
  @@ fun (node_ctxt, _block_hash) outbox_level message_index () ->
  let open Lwt_result_syntax in
  let+ commitment, proof =
    Outbox.proof_of_output_simple node_ctxt ~outbox_level ~message_index
  in
  (commitment, proof)

let () =
  Block_directory.register0 Sc_rollup_services.Block.simulate
  @@
  fun (node_ctxt, block)
      ()
      {messages; reveal_pages; insight_requests; log_kernel_debug_file}
    ->
  simulate_messages
    node_ctxt
    block
    ~reveal_pages
    ~insight_requests
    ~log_kernel_debug_file
    messages

let block_directory (node_ctxt : _ Node_context.t) =
  let module PVM = (val Pvm_rpc.of_kind node_ctxt.kind) in
  List.fold_left
    (fun dir f -> Tezos_rpc.Directory.merge dir (f node_ctxt))
    Tezos_rpc.Directory.empty
    [
      Rpc_directory.build_block_subdirectory;
      Block_directory.build_sub_directory;
      Block_helpers_directory.build_sub_directory;
      PVM.build_sub_directory;
    ]

let directory (node_ctxt : _ Node_context.t) =
  Tezos_rpc.Directory.merge
    (Octez_smart_rollup_node.Rpc_directory.top_directory node_ctxt)
    (Tezos_rpc.Directory.prefix
       Sc_rollup_services.Block.prefix
       (block_directory node_ctxt))
