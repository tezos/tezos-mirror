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

let msg : string =
  "In proxy mode, all RPCs of protocol genesis should be forwarded to the \
   node. Hence this code should not be reached, because the RPC directory is \
   empty."

module ProtoRpc : Tezos_proxy.Proxy_proto.PROTO_RPC = struct
  let split_key _ _ = None

  let failure_is_permanent _ = false

  let do_rpc (pgi : Tezos_proxy.Proxy.proxy_getter_input)
      (key : Tezos_protocol_environment.Proxy_context.M.key) =
    let open Lwt_result_syntax in
    let chain = pgi.chain in
    let block = pgi.block in
    let*! () =
      Tezos_proxy.Logger.emit
        Tezos_proxy.Logger.proxy_block_rpc
        ( Tezos_shell_services.Block_services.chain_to_string chain,
          Tezos_shell_services.Block_services.to_string block,
          key )
    in
    let* (raw_context : Tezos_context_sigs.Context.Proof_types.raw_context) =
      Protocol_client_context.Genesis_block_services.Context.read
        pgi.rpc_context
        ~chain
        ~block
        key
    in
    let*! () =
      Tezos_proxy.Logger.emit Tezos_proxy.Logger.tree_received
      @@ Int64.of_int (Tezos_proxy.Proxy_getter.raw_context_size raw_context)
    in
    return raw_context
end

let () =
  let open Tezos_proxy.Registration in
  let module M : Proxy_sig = struct
    module Protocol = Protocol_client_context.Lifted_protocol

    let protocol_hash = Protocol.hash

    let directory = Tezos_rpc.Directory.empty

    let initial_context (ctx : Tezos_proxy.Proxy_getter.rpc_context_args)
        (hash : Context_hash.t) =
      let open Lwt_result_syntax in
      let p_rpc = (module ProtoRpc : Tezos_proxy.Proxy_proto.PROTO_RPC) in
      let* (module ProxyDelegation) =
        Tezos_proxy.Proxy_getter.make_delegate ctx p_rpc hash
      in
      return
        (Tezos_protocol_environment.Proxy_context.empty
        @@ Some (module ProxyDelegation))

    let merkle_tree _ _ _ = failwith "%s" msg

    let time_between_blocks _ _ _ = failwith "%s" msg
  end in
  register_proxy_context (module M)
