(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_rpc_http
open Tezos_rpc_http_server
open Tezos_dal_node_services

let handle_split_slot ctxt fill slot =
  let open Lwt_result_syntax in
  let*? {dal_parameters; dal_constants; _} = Node_context.get_ready ctxt in
  let slot =
    if fill then
      Slot_manager.Utils.fill_x00 dal_parameters.Cryptobox.slot_size slot
    else slot
  in
  let store = Node_context.get_store ctxt in
  let+ commitment =
    Slot_manager.split_and_store
      store.slots_watcher
      dal_constants
      store.slots_store
      slot
  in
  Cryptobox.Commitment.to_b58check commitment

let handle_slot ctxt (_, commitment) trim () =
  let open Lwt_result_syntax in
  let*? {dal_parameters; dal_constants; _} = Node_context.get_ready ctxt in
  let* slot =
    Slot_manager.get_slot
      dal_parameters
      dal_constants
      (Node_context.get_store ctxt).slots_store
      commitment
  in
  let slot = if trim then Slot_manager.Utils.trim_x00 slot else slot in
  return slot

let handle_stored_slot_headers ctxt (_, block_hash) () () =
  let open Lwt_result_syntax in
  let*! shs =
    Slot_headers_store.list_secondary_keys_with_values
      (Node_context.get_store ctxt).slot_headers_store
      ~primary_key:block_hash
  in
  return @@ shs

let handle_slot_pages ctxt (_, commitment) () () =
  let open Lwt_result_syntax in
  let*? {dal_parameters; dal_constants; _} = Node_context.get_ready ctxt in
  Slot_manager.get_slot_pages
    dal_parameters
    dal_constants
    (Node_context.get_store ctxt).slots_store
    commitment

let handle_shard ctxt ((_, commitment), shard) () () =
  Slot_manager.get_shard
    (Node_context.get_store ctxt).slots_store
    commitment
    shard

let handle_monitor_slot_headers ctxt () () () =
  let stream, stopper = Store.open_slots_stream (Node_context.get_store ctxt) in
  let shutdown () = Lwt_watcher.shutdown stopper in
  let next () = Lwt_stream.get stream in
  Tezos_rpc.Answer.return_stream {next; shutdown}

let register_stored_slot_headers ctxt dir =
  Tezos_rpc.Directory.register
    dir
    (Services.stored_slot_headers ())
    (handle_stored_slot_headers ctxt)

let register_split_slot ctxt dir =
  Tezos_rpc.Directory.register0
    dir
    (Services.split_slot ())
    (handle_split_slot ctxt)

let register_show_slot ctxt dir =
  Tezos_rpc.Directory.register dir (Services.slot ()) (handle_slot ctxt)

let register_show_slot_pages ctxt dir =
  Tezos_rpc.Directory.register
    dir
    (Services.slot_pages ())
    (handle_slot_pages ctxt)

let shard_service = Services.shard ()

let register_shard ctxt dir =
  Tezos_rpc.Directory.register dir shard_service (handle_shard ctxt)

let shard_rpc ctxt commitment shard =
  Tezos_rpc.Context.make_call shard_service ctxt (((), commitment), shard) () ()

let monitor_slot_headers_service = Services.monitor_slot_headers ()

let register_monitor_slot_headers ctxt dir =
  Tezos_rpc.Directory.gen_register
    dir
    monitor_slot_headers_service
    (handle_monitor_slot_headers ctxt)

let monitor_slot_headers_rpc ctxt =
  Tezos_rpc.Context.make_streamed_call
    monitor_slot_headers_service
    ctxt
    ()
    ()
    ()

let register ctxt =
  Tezos_rpc.Directory.empty
  |> register_stored_slot_headers ctxt
  |> register_split_slot ctxt |> register_show_slot ctxt |> register_shard ctxt
  |> register_show_slot_pages ctxt
  |> register_monitor_slot_headers ctxt

let merge dir plugin_dir = Tezos_rpc.Directory.merge dir plugin_dir

let start configuration dir =
  let open Lwt_syntax in
  let Configuration.{rpc_addr; rpc_port; _} = configuration in
  let rpc_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default rpc_addr in
  let server =
    RPC_server.init_server dir ~acl ~media_types:Media_type.all_media_types
  in
  Lwt.catch
    (fun () ->
      let* () =
        RPC_server.launch
          ~host
          server
          ~callback:(RPC_server.resto_callback server)
          node
      in
      return_ok server)
    fail_with_exn

let shutdown = RPC_server.shutdown

let install_finalizer rpc_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = shutdown rpc_server in
  let* () = Event.(emit shutdown_node exit_status) in
  Tezos_base_unix.Internal_event_unix.close ()
