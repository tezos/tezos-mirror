(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let on_layer_1_chain_event node_ctxt store chain_event =
  let open Lwt_result_syntax in
  let* () = Inbox.update node_ctxt store chain_event in
  let* () = Interpreter.Arith.update store chain_event in
  let*! () = Layer1.processed chain_event in
  return ()

let iter_stream stream handle =
  let rec go () =
    Lwt.bind (Lwt_stream.get stream) @@ fun tok ->
    match tok with
    | None -> return_unit
    | Some element -> Lwt_result.bind (handle element) go
  in
  go ()

let daemonize node_ctxt store layer_1_chain_events =
  Lwt.no_cancel
  @@ iter_stream layer_1_chain_events
  @@ on_layer_1_chain_event node_ctxt store

let install_finalizer store rpc_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = RPC_server.Arith.shutdown rpc_server in
  let* () = Store.close store in
  let* () = Event.shutdown_node exit_status in
  Tezos_base_unix.Internal_event_unix.close ()

let run ~data_dir (cctxt : Protocol_client_context.full) =
  let open Lwt_result_syntax in
  let start () =
    let*! () = Event.starting_node () in
    let* configuration = Configuration.load ~data_dir in
    let open Configuration in
    let {rpc_addr; rpc_port; sc_rollup_address; sc_rollup_node_operator; _} =
      configuration
    in
    let*! store = Store.load configuration in
    let* rpc_server = RPC_server.Arith.start store configuration in
    let* node_ctxt =
      Node_context.init cctxt sc_rollup_address sc_rollup_node_operator
    in
    (* Check that the public key hash is valid *)
    let* (_pkh, _pk, _skh) = Node_context.get_operator_keys node_ctxt in
    (* Do not reorder the operations above this one, as
        State depends on the RPC server to fetch the initial
        rollup level, and modules below depend on State
        to fetch in-memory variables
    *)
    let* tezos_heads = Layer1.start configuration node_ctxt.cctxt store in
    let*! () = Inbox.start store node_ctxt in
    let* () = Interpreter.Arith.start store in
    let _ = install_finalizer store rpc_server in
    let*! () = Event.node_is_ready ~rpc_addr ~rpc_port in
    daemonize node_ctxt store tezos_heads
  in
  start ()
