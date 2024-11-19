(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

type t = {
  server : RPC_server.server;
  host : string;
  node : Conduit_lwt_unix.server;
  acl : Resto_acl.Acl.t;
}

module Acl = struct
  open Resto_acl.Acl

  let allow_all = RPC_server.Acl.allow_all

  let secure =
    Allow_all
      {
        except =
          List.map
            parse
            [
              "GET /global/block/*/durable/wasm_2_0_0/subkeys";
              "GET /global/block/*/durable/wasm_2_0_0/values";
              "/local/batcher/**";
              "/admin/**";
              "/stats/**";
              "/config";
            ];
      }

  let default (address : P2p_addr.t) =
    let open Ipaddr in
    if V6.scope address = Interface then allow_all else secure
end

let start ~rpc_addr ~rpc_port ~acl ~cors dir =
  let open Lwt_result_syntax in
  let rpc_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let node = `TCP (`Port rpc_port) in
  let*! acl_policy = RPC_server.Acl.resolve_domain_names acl in
  let acl =
    RPC_server.Acl.find_policy acl_policy (host, Some rpc_port)
    |> Option.value_f ~default:(fun () -> Acl.default rpc_addr)
  in
  let server =
    RPC_server.init_server
      dir
      ~cors
      ~acl
      ~media_types:Media_type.all_media_types
  in
  protect @@ fun () ->
  let*! () =
    RPC_server.launch
      ~host
      server
      ~callback:(RPC_server.resto_callback server)
      node
  in
  return {server; host; node; acl}

let shutdown {server; _} = RPC_server.shutdown server
