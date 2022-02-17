(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
open Protocol
open Tezos_rpc
open Tezos_rpc_http
open Tezos_rpc_http_server

let current_tezos_head =
  RPC_service.get_service
    ~description:"Get the current head stored in the tx-rollup-node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Block_hash.encoding)
    RPC_path.(open_root / "tezos_head")

let current_inbox =
  RPC_service.get_service
    ~description:"Get the current inbox stored in the tx-rollup-node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Alpha_context.Tx_rollup_inbox.encoding)
    RPC_path.(open_root / "current_inbox")

let register_current_tezos_head state dir =
  RPC_directory.register0 dir current_tezos_head (fun () () ->
      State.get_head state >|= ok)

let register_current_inbox state dir =
  RPC_directory.register0 dir current_inbox (fun () () ->
      State.get_head state >|= ok >>=? function
      | None -> return None
      | Some hash ->
          State.find_inbox state hash >|= fun x ->
          ok (Option.map Inbox.to_protocol_inbox x))

let register state =
  RPC_directory.empty
  |> register_current_tezos_head state
  |> register_current_inbox state

let launch ~host ~acl ~node ~dir () =
  RPC_server.launch ~media_types:Media_type.all_media_types ~host ~acl node dir
  >>= return

let start configuration state =
  let Configuration.{rpc_addr; rpc_port; _} = configuration in
  let addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string addr in
  let dir = register state in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default addr in
  Lwt.catch (launch ~host ~acl ~node ~dir) fail_with_exn
