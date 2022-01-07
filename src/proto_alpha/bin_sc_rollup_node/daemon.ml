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

(* For the moment, the daemon does nothing. *)
let daemonize () = Lwt_utils.never_ending ()

let install_finalizer rpc_server =
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  RPC_server.shutdown rpc_server >>= fun () ->
  Event.shutdown_node exit_status >>= Internal_event_unix.close

let run ~data_dir (cctxt : Protocol_client_context.full) =
  let start () =
    Event.starting_node () >>= fun () ->
    Configuration.load ~data_dir >>=? fun configuration ->
    let Configuration.{rpc_addr; rpc_port; _} = configuration in
    Layer1.start configuration cctxt >>=? fun () ->
    RPC_server.start configuration >>=? fun rpc_server ->
    let _ = install_finalizer rpc_server in
    Event.node_is_ready ~rpc_addr ~rpc_port >>= daemonize
  in
  Lwt.catch start fail_with_exn
