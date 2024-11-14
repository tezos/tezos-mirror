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

module ProtoRpc : Tezos_proxy.Proxy_proto.PROTO_RPC = struct
  (** Split that is always done, no matter the mode *)
  let split_always key =
    match key with
    (* matches paths like:
       contracts/index/000002298c03ed7d454a101eb7022bc95f7e5f41ac78/tail *)
    | "contracts" :: "index" :: i :: tail ->
        Some (["contracts"; "index"; i], tail)
    | "cycle" :: i :: tail -> Some (["cycle"; i], tail)
    (* matches paths like:
       rolls/owner/snapshot/19/1/tail *)
    | "rolls" :: "owner" :: "snapshot" :: i :: j :: tail ->
        Some (["rolls"; "owner"; "snapshot"; i; j], tail)
    | "v1" :: tail -> Some (["v1"], tail)
    | _ -> None

  let split_key (key : Tezos_protocol_environment.Proxy_context.M.key) :
      (Tezos_protocol_environment.Proxy_context.M.key
      * Tezos_protocol_environment.Proxy_context.M.key)
      option =
    split_always key

  let failure_is_permanent = function
    | ["pending_migration_balance_updates"]
    | ["pending_migration_operation_results"] ->
        true
    | _ -> false

  let do_rpc (pgi : Tezos_proxy.Proxy.proxy_getter_input)
      (key : Tezos_protocol_environment.Proxy_context.M.key) =
    let chain = pgi.chain in
    let block = pgi.block in
    Tezos_proxy.Logger.emit
      Tezos_proxy.Logger.proxy_block_rpc
      ( Tezos_shell_services.Block_services.chain_to_string chain,
        Tezos_shell_services.Block_services.to_string block,
        key )
    >>= fun () ->
    Protocol_client_context.Alpha_block_services.Context.read
      pgi.rpc_context
      ~chain
      ~block
      key
    >>=? fun (raw_context : Tezos_context_sigs.Context.Proof_types.raw_context)
      ->
    Tezos_proxy.Logger.emit Tezos_proxy.Logger.tree_received
    @@ Int64.of_int (Tezos_proxy.Proxy_getter.raw_context_size raw_context)
    >>= fun () -> return raw_context
end

let initial_context (ctx : Tezos_proxy.Proxy_getter.rpc_context_args)
    (hash : Context_hash.t) :
    Tezos_protocol_environment.Context.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*! () =
    Tezos_proxy.Logger.emit
      Tezos_proxy.Logger.proxy_getter_created
      ( Tezos_shell_services.Block_services.chain_to_string ctx.chain,
        Tezos_shell_services.Block_services.to_string ctx.block )
  in
  let p_rpc = (module ProtoRpc : Tezos_proxy.Proxy_proto.PROTO_RPC) in
  let* (module ProxyDelegation) =
    Tezos_proxy.Proxy_getter.make_delegate ctx p_rpc hash
  in
  let empty =
    Tezos_protocol_environment.Proxy_context.empty
    @@ Some (module ProxyDelegation)
  in
  let version_value = "hangzhou_011" in
  let*! ctxt =
    Tezos_protocol_environment.Context.add
      empty
      ["version"]
      (Bytes.of_string version_value)
  in
  Lwt_result.ok (Protocol.Main.init_context ctxt)

let time_between_blocks (rpc_context : Tezos_rpc.Context.generic)
    (chain : Tezos_shell_services.Block_services.chain)
    (block : Tezos_shell_services.Block_services.block) =
  let open Protocol in
  let rpc_context = new Protocol_client_context.wrap_rpc_context rpc_context in
  Constants_services.all rpc_context (chain, block) >>=? fun constants ->
  let times = constants.parametric.time_between_blocks in
  return @@ Option.map Alpha_context.Period.to_seconds (List.hd times)

let () =
  let open Tezos_proxy.Registration in
  let module M : Proxy_sig = struct
    module Protocol = Protocol_client_context.Lifted_protocol

    let protocol_hash = Protocol.hash

    let directory = Plugin.RPC.rpc_services

    let initial_context = initial_context

    let time_between_blocks = time_between_blocks

    include Light.M
  end in
  register_proxy_context (module M)
