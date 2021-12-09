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

open Tezos_rpc
open Tezos_rpc_http
open Tezos_rpc_http_server
open Protocol
open Alpha_context

module S = struct
  let sc_rollup_address =
    RPC_service.get_service
      ~description:"Smart-contract rollup address"
      ~query:RPC_query.empty
      ~output:Sc_rollup.Address.encoding
      RPC_path.(open_root / "sc_rollup_address")
end

let register_sc_rollup_address configuration dir =
  RPC_directory.register0 dir S.sc_rollup_address (fun () () ->
      return @@ configuration.Configuration.sc_rollup_address)

let register configuration =
  RPC_directory.empty |> register_sc_rollup_address configuration

let start configuration =
  let Configuration.{rpc_addr; rpc_port; _} = configuration in
  let rpc_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let dir = register configuration in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.default rpc_addr in
  Lwt.catch
    (fun () ->
      RPC_server.launch
        ~media_types:Media_type.all_media_types
        ~host
        ~acl
        node
        dir
      >>= return)
    fail_with_exn

let shutdown = RPC_server.shutdown
