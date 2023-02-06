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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4750
   Move this to RPC_server.Legacy once all operating modes are supported. *)
let start_legacy ~rpc_address ~rpc_port ~reveal_data_dir ~threshold cctxt ctxt
    dac_pks_opt dac_sk_uris =
  let open Lwt_syntax in
  let dir =
    Tezos_rpc.Directory.register_dynamic_directory
      Tezos_rpc.Directory.empty
      Tezos_rpc.Path.open_root
      (fun () ->
        match Node_context.get_status ctxt with
        | Ready {dac_plugin = (module Dac_plugin); _} ->
            Lwt.return
              (Dac_plugin.RPC.rpc_services
                 ~reveal_data_dir
                 cctxt
                 dac_pks_opt
                 dac_sk_uris
                 threshold)
        | Starting -> Lwt.return Tezos_rpc.Directory.empty)
  in
  let rpc_address = P2p_addr.of_string_exn rpc_address in
  let host = Ipaddr.V6.to_string rpc_address in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default rpc_address in
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
