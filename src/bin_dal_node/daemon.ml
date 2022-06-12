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

module RPC_server = struct
  let register _store _configuration = RPC_directory.empty

  let start configuration dir =
    let open Lwt_syntax in
    let Configuration.{rpc_addr; rpc_port; _} = configuration in
    let rpc_addr = P2p_addr.of_string_exn rpc_addr in
    let host = Ipaddr.V6.to_string rpc_addr in
    let node = `TCP (`Port rpc_port) in
    let acl = RPC_server.Acl.default rpc_addr in
    Lwt.catch
      (fun () ->
        let* server =
          RPC_server.launch
            ~media_types:Media_type.all_media_types
            ~host
            ~acl
            node
            dir
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
end

let run ~data_dir _ctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* config = Configuration.load ~data_dir in
  let config = {config with data_dir} in
  let*! store = Store.init config in
  let* rpc_server = RPC_server.(start config (register config store)) in
  let _ = RPC_server.install_finalizer rpc_server in
  let*! () =
    Event.(emit rpc_server_is_ready (config.rpc_addr, config.rpc_port))
  in
  let*! () = Event.(emit node_is_ready ()) in
  Lwt_utils.never_ending ()
