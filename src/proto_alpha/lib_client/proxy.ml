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

module L = ( val Tezos_proxy.Logger.logger ~protocol_name:Protocol.name
               : Tezos_proxy.Logger.S )

let proxy_block_header (rpc_context : RPC_context.json)
    (chain : Tezos_shell_services.Block_services.chain)
    (block : Tezos_shell_services.Block_services.block) =
  let rpc_context = new Protocol_client_context.wrap_rpc_context rpc_context in
  L.emit
    L.proxy_block_header
    ( Tezos_shell_services.Block_services.chain_to_string chain,
      Tezos_shell_services.Block_services.to_string block )
  >>= fun () ->
  Protocol_client_context.Alpha_block_services.header
    rpc_context
    ~chain
    ~block
    ()

module ProtoRpc : Tezos_proxy.Proxy_proto.PROTO_RPC = struct
  let split_key (key : Proxy_context.M.key) :
      (Proxy_context.M.key * Proxy_context.M.key) option =
    match key with
    (* matches paths like:
       contracts/index/05/37/bc/fb/1e/39/000002298c03ed7d454a101eb7022bc95f7e5f41ac78/tail *)
    | "contracts"
      :: index
         :: hash0 :: hash1 :: hash2 :: hash3 :: hash4 :: hash5 :: id :: tail ->
        Some
          ( ["contracts"; index; hash0; hash1; hash2; hash3; hash4; hash5; id],
            tail )
    | "cycle" :: i :: tail ->
        Some (["cycle"; i], tail)
    (* matches paths like:
       rolls/owner/snapshot/19/1/tail *)
    | "rolls" :: "owner" :: "snapshot" :: i :: j :: tail ->
        Some (["rolls"; "owner"; "snapshot"; i; j], tail)
    | "v1" :: tail ->
        Some (["v1"], tail)
    | _ ->
        None

  let do_rpc (pgi : Tezos_proxy.Proxy.proxy_getter_input)
      (key : Proxy_context.M.key) =
    let chain = pgi.chain in
    let block = pgi.block in
    L.emit
      L.proxy_block_rpc
      ( Tezos_shell_services.Block_services.chain_to_string chain,
        Tezos_shell_services.Block_services.to_string block,
        key )
    >>= fun () ->
    Protocol_client_context.Alpha_block_services.Context.read
      pgi.rpc_context
      ~chain
      ~block
      key
    >>=? fun (raw_context : Block_services.raw_context) ->
    L.emit L.tree_received
    @@ Int64.of_int (Tezos_proxy.Proxy_getter.raw_context_size raw_context)
    >>= fun () -> return raw_context
end

let initial_context
    (proxy_builder :
      Tezos_proxy.Proxy_proto.proto_rpc ->
      Tezos_proxy.Proxy_getter.proxy_m Lwt.t) (rpc_context : RPC_context.json)
    (chain : Tezos_shell_services.Block_services.chain)
    (block : Tezos_shell_services.Block_services.block) :
    Environment_context.Context.t Lwt.t =
  let p_rpc = (module ProtoRpc : Tezos_proxy.Proxy_proto.PROTO_RPC) in
  proxy_builder p_rpc
  >>= fun (module M) ->
  L.emit
    L.proxy_getter_created
    ( Tezos_shell_services.Block_services.chain_to_string chain,
      Tezos_shell_services.Block_services.to_string block )
  >>= fun () ->
  let pgi : Tezos_proxy.Proxy.proxy_getter_input =
    {rpc_context = (rpc_context :> RPC_context.simple); chain; block}
  in
  let module N : Proxy_context.M.ProxyDelegate = struct
    let proxy_dir_mem = M.proxy_dir_mem pgi

    let proxy_get = M.proxy_get pgi

    let proxy_mem = M.proxy_mem pgi
  end in
  let empty = Proxy_context.empty @@ Some (module N) in
  let version_value = "alpha_current" in
  Tezos_protocol_environment.Context.add
    empty
    ["version"]
    (Bytes.of_string version_value)

let init_env_rpc_context (_printer : Tezos_client_base.Client_context.printer)
    (proxy_builder :
      Tezos_proxy.Proxy_proto.proto_rpc ->
      Tezos_proxy.Proxy_getter.proxy_m Lwt.t) (rpc_context : RPC_context.json)
    (chain : Tezos_shell_services.Block_services.chain)
    (block : Tezos_shell_services.Block_services.block) :
    Tezos_protocol_environment.rpc_context tzresult Lwt.t =
  proxy_block_header rpc_context chain block
  >>=? fun {shell; hash; _} ->
  let block_hash = hash in
  initial_context proxy_builder rpc_context chain block
  >>= fun context ->
  return {Tezos_protocol_environment.block_hash; block_header = shell; context}

let () =
  let open Tezos_proxy.Registration in
  let module M : Proxy_sig = struct
    module Protocol = Protocol_client_context.Lifted_protocol

    let protocol_hash = Protocol.hash

    let directory = Plugin.RPC.rpc_services

    let hash = Protocol_client_context.Alpha_block_services.hash

    let init_env_rpc_context = init_env_rpc_context

    include Light.M
  end in
  register_proxy_context (module M)
