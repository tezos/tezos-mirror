(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

open Tezos_rpc_http_server
open RPC_directory_helpers
open Octez_smart_rollup_node

module Local_directory = Make_directory (struct
  include Sc_rollup_services.Local

  type context = Node_context.rw

  type subcontext = unit

  let context_of_prefix _ll_node_ctxt () = Lwt_result_syntax.return_unit
end)

(* Register server handlers *)
let () =
  let open Protocol.Alpha_context.Sc_rollup in
  ( Local_directory.register0
      (Sequencer_services.Local.durable_state_value Kind.Wasm_2_0_0)
  @@ fun () {key} () -> Seq_batcher.get_simulated_state_value key ) ;

  ( Local_directory.register0
      (Sequencer_services.Local.durable_state_subkeys Kind.Wasm_2_0_0)
  @@ fun () {key} () -> Seq_batcher.get_simulated_state_subkeys key ) ;

  Local_directory.register0 Sc_rollup_services.Local.injection
  @@ fun _node_ctxt () messages -> Seq_batcher.register_messages messages

let register (node_ctxt : _ Node_context.t) =
  List.fold_left
    (fun dir f -> Tezos_rpc.Directory.merge dir (f node_ctxt))
    Tezos_rpc.Directory.empty
    [Local_directory.build_directory]

let start (node_ctxt : _ Node_context.t) configuration =
  let open Lwt_result_syntax in
  let Configuration.{rpc_addr; rpc_port; _} = configuration in
  let rpc_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let dir = register node_ctxt in
  let server =
    RPC_server.init_server
      dir
      ~acl
      ~media_types:Tezos_rpc_http.Media_type.all_media_types
  in
  protect @@ fun () ->
  let*! () =
    RPC_server.launch
      ~host
      server
      ~callback:(RPC_server.resto_callback server)
      node
  in
  return server

let shutdown = RPC_server.shutdown
