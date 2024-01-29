(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module Bare_context = struct
  module Tree = Irmin_context.Tree

  type t = Irmin_context.rw

  type index = Irmin_context.rw_index

  type nonrec tree = Irmin_context.tree

  let init ?patch_context:_ ?readonly:_ ?index_log_size:_ path =
    let open Lwt_syntax in
    let* res = Irmin_context.load ~cache_size:100_000 Read_write path in
    match res with
    | Ok res -> return res
    | Error _ -> Lwt.fail_with "could not initialize the context"

  let empty index = Irmin_context.empty index
end

type t = Irmin_context.PVMState.value

module Wasm_utils =
  Wasm_utils.Make (Tezos_tree_encoding.Encodings_util.Make (Bare_context))
module Wasm = Wasm_debugger.Make (Wasm_utils)

let execute ~config evm_state inbox =
  let open Lwt_result_syntax in
  let inbox = List.map (function `Input s -> s) inbox in
  let inbox = List.to_seq [inbox] in
  let* evm_state, _, _, _ =
    Wasm.Commands.eval 0l inbox config Inbox evm_state
  in
  return evm_state

let init ~kernel =
  let open Lwt_result_syntax in
  let evm_state = Irmin_context.PVMState.empty () in
  let* evm_state =
    Wasm.start ~tree:evm_state Tezos_scoru_wasm.Wasm_pvm_state.V3 kernel
  in
  return evm_state

let modify ~key ~value evm_state = Wasm.set_durable_value evm_state key value

let inspect evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* value = Wasm.Commands.find_key_in_durable evm_state key in
  Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_bytes value

let current_block_height evm_state =
  let open Lwt_syntax in
  let* current_block_number =
    inspect evm_state Durable_storage_path.Block.current_number
  in
  match current_block_number with
  | None ->
      (* No block has been created yet and we are waiting for genesis,
         whose number will be [zero]. Since the semantics of [apply_blueprint]
         is to verify the block height has been incremented once, we default to
         [-1]. *)
      return (Block_height Z.(pred zero))
  | Some current_block_number ->
      let (Qty current_block_number) = decode_number current_block_number in
      return (Block_height current_block_number)

let execute_and_inspect ~config
    ~input:Simulation.Encodings.{messages; insight_requests; _} ctxt =
  let open Lwt_result_syntax in
  let keys =
    List.map
      (function
        | Simulation.Encodings.Durable_storage_key l ->
            "/" ^ String.concat "/" l
        (* We use only `Durable_storage_key` in simulation. *)
        | _ -> assert false)
      insight_requests
  in
  (* Messages from simulation requests are already valid inputs. *)
  let messages = List.map (fun s -> `Input s) messages in
  let* evm_state = execute ~config ctxt messages in
  let*! values = List.map_p (fun key -> inspect evm_state key) keys in
  return values

type error += Cannot_apply_blueprint

let apply_blueprint ~config evm_state (blueprint : Blueprint_types.payload) =
  let open Lwt_result_syntax in
  let exec_inputs =
    List.map
      (function `External payload -> `Input ("\001" ^ payload))
      blueprint
  in
  let*! (Block_height before_height) = current_block_height evm_state in
  let* evm_state = execute ~config evm_state exec_inputs in
  let*! (Block_height after_height) = current_block_height evm_state in
  if Z.(equal (succ before_height) after_height) then
    return (evm_state, Block_height after_height)
  else tzfail Cannot_apply_blueprint
