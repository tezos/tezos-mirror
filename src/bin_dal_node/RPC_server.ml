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

module Slots_handlers = struct
  let call_handler handler ctxt =
    let open Lwt_result_syntax in
    let*? {dal_constants = cryptobox; _} = Node_context.get_ready ctxt in
    let store = Node_context.get_store ctxt in
    handler store cryptobox

  let post_slots ctxt () slot = call_handler (Slot_manager.add_slots slot) ctxt
end

let add_service registerer service handler directory =
  registerer directory service handler

let register_new :
    Node_context.t -> unit Tezos_rpc.Directory.t -> unit Tezos_rpc.Directory.t =
 fun ctxt directory ->
  directory
  |> add_service
       Tezos_rpc.Directory.register0
       Services.post_slots
       (Slots_handlers.post_slots ctxt)

let register_legacy ctxt =
  let open RPC_server_legacy in
  Tezos_rpc.Directory.empty
  |> register_stored_slot_headers ctxt
  |> register_split_slot ctxt |> register_show_slot ctxt |> register_shard ctxt
  |> register_shards ctxt
  |> register_show_slot_pages ctxt
  |> register_monitor_slot_headers ctxt

let register ctxt = register_new ctxt (register_legacy ctxt)

let merge dir plugin_dir = Tezos_rpc.Directory.merge dir plugin_dir

let start configuration ctxt =
  let open Lwt_syntax in
  let Configuration.{rpc_addr; rpc_port; dac = {reveal_data_dir; _}; _} =
    configuration
  in
  let dir = register ctxt in
  let plugin_prefix = Tezos_rpc.Path.(open_root / "plugin") in
  let dir =
    Tezos_rpc.Directory.register_dynamic_directory dir plugin_prefix (fun () ->
        match Node_context.get_status ctxt with
        | Ready {plugin = (module Plugin); _} ->
            Lwt.return (Plugin.RPC.rpc_services ~reveal_data_dir)
        | Starting -> Lwt.return Tezos_rpc.Directory.empty)
  in
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
