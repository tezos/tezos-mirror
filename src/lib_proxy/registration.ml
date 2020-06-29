(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let check_client_node_proto_agree (rpc_context : #RPC_context.simple)
    (proto_hash : Protocol_hash.t option)
    (chain : Tezos_shell_services.Block_services.chain)
    (block : Tezos_shell_services.Block_services.block) : unit tzresult Lwt.t =
  match proto_hash with
  | None ->
      return_unit
  | Some proto_hash ->
      Tezos_shell_services.Block_services.protocols
        rpc_context
        ~chain
        ~block
        ()
      >>=? fun {current_protocol; _} ->
      if Protocol_hash.equal current_protocol proto_hash then return_unit
      else
        failwith
          "Protocol passed to the proxy (%a) and protocol of the node (%a) \
           differ."
          Protocol_hash.pp
          proto_hash
          Protocol_hash.pp
          current_protocol

module type Proxy_sig = sig
  val protocol_hash : Protocol_hash.t

  (** RPCs provided by the protocol *)
  val directory : Tezos_protocol_environment.rpc_context RPC_directory.t

  (** How to build the context to execute RPCs on *)
  val init_env_rpc_context :
    Tezos_client_base.Client_context.printer ->
    RPC_context.json ->
    Tezos_shell_services.Block_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Tezos_protocol_environment.rpc_context tzresult Lwt.t
end

type proxy_environment = (module Proxy_sig)

let registered : proxy_environment list ref = ref []

let register_proxy_context m =
  let (module INCOMING_P : Proxy_sig) = m in
  if
    List.exists
      (fun (module P : Proxy_sig) ->
        Protocol_hash.(P.protocol_hash = INCOMING_P.protocol_hash))
      !registered
  then
    raise
    @@ Invalid_argument
         (Format.asprintf
            "A proxy environment for protocol %a is registered already"
            Protocol_hash.pp
            INCOMING_P.protocol_hash)
  else registered := m :: !registered

let get_registered_proxy (_printer : Tezos_client_base.Client_context.printer)
    (rpc_context : #RPC_context.simple)
    (protocol_hash_opt : Protocol_hash.t option)
    (chain : Tezos_shell_services.Block_services.chain)
    (block : Tezos_shell_services.Block_services.block) :
    proxy_environment tzresult Lwt.t =
  check_client_node_proto_agree rpc_context protocol_hash_opt chain block
  >>=? fun _ ->
  let available = !registered in
  match available with
  | [] ->
      failwith "get_registered_proxy: no registered proxy environment"
  | fst_proxy :: _ -> (
    match protocol_hash_opt with
    | None ->
        return fst_proxy
    | Some protocol_hash -> (
        let proxy_opt =
          List.find_opt
            (fun (module Proxy : Proxy_sig) ->
              Protocol_hash.equal protocol_hash Proxy.protocol_hash)
            available
        in
        match proxy_opt with
        | Some proxy ->
            return proxy
        | None ->
            failwith
              "requested protocol not found in available proxy environments" )
    )
