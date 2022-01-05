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

open Tezos_shell_services

let check_client_node_proto_agree (rpc_context : #RPC_context.simple)
    (proto_hash : Protocol_hash.t) (chain : Block_services.chain)
    (block : Block_services.block) : unit tzresult Lwt.t =
  let open Lwt_tzresult_syntax in
  let* {current_protocol; _} =
    Block_services.protocols rpc_context ~chain ~block ()
  in
  if Protocol_hash.equal current_protocol proto_hash then return_unit
  else
    failwith
      "Protocol passed to the proxy (%a) and protocol of the node (%a) differ."
      Protocol_hash.pp
      proto_hash
      Protocol_hash.pp
      current_protocol

let get_node_protocol (rpc_context : #RPC_context.simple)
    (chain : Block_services.chain) (block : Block_services.block) :
    Protocol_hash.t tzresult Lwt.t =
  let open Lwt_tzresult_syntax in
  let* {current_protocol; _} =
    Block_services.protocols rpc_context ~chain ~block ()
  in
  return current_protocol

module type Proxy_sig = sig
  val protocol_hash : Protocol_hash.t

  (** RPCs provided by the protocol *)
  val directory : Tezos_protocol_environment.rpc_context RPC_directory.t

  (** The protocol's /chains/<chain>/blocks/<block_id>/hash RPC *)
  val hash :
    #RPC_context.simple ->
    ?chain:Block_services.chain ->
    ?block:Block_services.block ->
    unit ->
    Block_hash.t tzresult Lwt.t

  (** How to build the context to execute RPCs on *)
  val init_env_rpc_context :
    Tezos_client_base.Client_context.printer ->
    (Proxy_proto.proto_rpc -> Proxy_getter.proxy_m Lwt.t) ->
    RPC_context.generic ->
    Proxy.mode ->
    Block_services.chain ->
    Block_services.block ->
    Tezos_protocol_environment.rpc_context tzresult Lwt.t

  val time_between_blocks :
    RPC_context.generic ->
    Block_services.chain ->
    Block_services.block ->
    int64 option tzresult Lwt.t

  include Light_proto.PROTO_RPCS
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

let get_all_registered () : proxy_environment list = !registered

let get_registered_proxy (printer : Tezos_client_base.Client_context.printer)
    (rpc_context : #RPC_context.simple) (mode : [< `Mode_light | `Mode_proxy])
    ?(chain = `Main) ?(block = `Head 0)
    (protocol_hash_opt : Protocol_hash.t option) :
    proxy_environment tzresult Lwt.t =
  let open Lwt_tzresult_syntax in
  let mode_str =
    match mode with `Mode_light -> "light mode" | `Mode_proxy -> "proxy"
  in
  let* protocol_hash =
    match protocol_hash_opt with
    | None ->
        let* protocol_hash = get_node_protocol rpc_context chain block in
        let*! () =
          printer#warning
            "protocol of %s unspecified, using the node's protocol: %a"
            mode_str
            Protocol_hash.pp
            protocol_hash
        in
        return protocol_hash
    | Some protocol_hash -> return protocol_hash
  in
  let* () =
    check_client_node_proto_agree rpc_context protocol_hash chain block
  in
  let available = !registered in
  let proxy_opt =
    List.find_opt
      (fun (module Proxy : Proxy_sig) ->
        Protocol_hash.equal protocol_hash Proxy.protocol_hash)
      available
  in
  match proxy_opt with
  | Some proxy -> return proxy
  | None -> (
      match available with
      | [] ->
          failwith
            "There are no proxy environments registered. --mode proxy cannot \
             be honored."
      | fst_available :: _ ->
          let (module Proxy : Proxy_sig) = fst_available in
          let fst_available_proto = Proxy.protocol_hash in
          let*! () =
            printer#warning
              "requested protocol (%a) not found in available proxy \
               environments: %a@;\
               Proceeding with the first available protocol (%a). This will \
               work if the mismatch is harmless, otherwise deserialization is \
               the failure most likely to happen."
              Protocol_hash.pp
              protocol_hash
              (Format.pp_print_list
                 ~pp_sep:Format.pp_print_space
                 Protocol_hash.pp)
              ((List.map (fun (module P : Proxy_sig) -> P.protocol_hash))
                 available)
              Protocol_hash.pp
              fst_available_proto
          in
          return fst_available)
