(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module TreeEncoding =
  Wasm_utils.Make
    (Tezos_tree_encoding.Encodings_util.Make (Sequencer_context.Context))
module Wasm = Wasm_debugger.Make (TreeEncoding)

let execute ?(commit = false) ctxt inbox =
  let open Lwt_result_syntax in
  let inbox = List.map (function `Input s -> s) inbox in
  let inbox = List.to_seq [inbox] in
  let config =
    Config.config
      ~preimage_directory:ctxt.Sequencer_context.preimages
      ~kernel_debug:true
      ~destination:ctxt.Sequencer_context.smart_rollup_address
      ()
  in
  let* evm_state, _, _, _ =
    Wasm.Commands.eval 0l inbox config Inbox ctxt.Sequencer_context.evm_state
  in
  let* ctxt =
    if commit then Sequencer_context.commit ctxt evm_state else return ctxt
  in
  return (ctxt, evm_state)

let init ~smart_rollup_address ~rollup_node_endpoint ctxt =
  let open Lwt_result_syntax in
  let* evm_state =
    Wasm.start
      ~tree:ctxt.Sequencer_context.evm_state
      Tezos_scoru_wasm.Wasm_pvm_state.V3
      ctxt.kernel
  in
  let ctxt =
    {ctxt with evm_state; next_blueprint_number = Ethereum_types.(Qty Z.one)}
  in
  (* Create the first empty block. *)
  let inputs =
    Sequencer_blueprint.create
      ~smart_rollup_address
      ~transactions:[]
      ~number:Ethereum_types.(Qty Z.zero)
  in
  let* () = Rollup_node_services.publish ~rollup_node_endpoint inputs in
  let inputs =
    List.map (function `External payload -> `Input ("\001" ^ payload)) inputs
  in
  let* ctxt, _evm_state = execute ~commit:true ctxt inputs in
  return ctxt

let inspect evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* value = Wasm.Commands.find_key_in_durable evm_state key in
  Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_bytes value

let execute_and_inspect ctxt
    ~input:Simulation.Encodings.{messages; insight_requests; _} =
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
  let* _ctxt, evm_state = execute ctxt messages in
  let*! values = List.map_p (fun key -> inspect evm_state key) keys in
  return values
