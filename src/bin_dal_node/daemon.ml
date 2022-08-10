(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

let daemonize cctxt handle = Lwt.no_cancel @@ Layer1.iter_events cctxt handle

let resolve_plugin cctxt =
  let open Lwt_result_syntax in
  let* protocols =
    Tezos_shell_services.Chain_services.Blocks.protocols cctxt ()
  in
  return
  @@ Option.either
       (Dal_constants_plugin.get protocols.current_protocol)
       (Dal_constants_plugin.get protocols.next_protocol)

let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* config = Configuration.load ~data_dir in
  let config = {config with data_dir} in
  let*! store = Store.init config in
  let ready = ref false in
  let*! () = Event.(emit layer1_node_tracking_started ()) in
  daemonize cctxt @@ fun (_hash, (_block_header : Tezos_base.Block_header.t)) ->
  if not !ready then
    let* plugin = resolve_plugin cctxt in
    match plugin with
    | Some plugin ->
        let (module Plugin : Dal_constants_plugin.T) = plugin in
        let*! () =
          Event.(
            emit
              protocol_plugin_resolved
              (Format.asprintf "%a" Protocol_hash.pp_short Plugin.Proto.hash))
        in
        let* dal_constants, dal_parameters =
          Cryptobox.init config.unsafe_srs cctxt plugin
        in
        let ctxt = Node_context.make config dal_constants dal_parameters in
        let* rpc_server = RPC_server.(start config (register ctxt store)) in
        let _ = RPC_server.install_finalizer rpc_server in
        let*! () =
          Event.(emit rpc_server_is_ready (config.rpc_addr, config.rpc_port))
        in
        let*! () = Event.(emit node_is_ready ()) in
        ready := true ;
        return_unit
    | None -> return_unit
  else return_unit
