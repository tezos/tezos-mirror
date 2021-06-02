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

module type BLOCK_TO_HASH = sig
  (** A [None] result value means the caller should use the block passed
      as third argument, even if not identified by a hash *)
  val hash_of_block :
    #RPC_context.simple ->
    Tezos_shell_services.Shell_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Block_hash.t option tzresult Lwt.t

  val add :
    Tezos_shell_services.Shell_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Block_hash.t ->
    unit
end

module Hashtbl = Stdlib.Hashtbl

module BlockToHash (S : Registration.Proxy_sig) : BLOCK_TO_HASH = struct
  let table = Hashtbl.create 17

  let add chain block hash = Hashtbl.add table (chain, block) hash

  let hash_of_block (rpc_context : #RPC_context.simple)
      (chain : Tezos_shell_services.Shell_services.chain)
      (block : Tezos_shell_services.Block_services.block) =
    match Light.hash_of_block block with
    | Some h ->
        (* Block is defined by its hash *)
        return_some h
    | None -> (
        match Hashtbl.find_opt table (chain, block) with
        | Some hash ->
            (* Result is in cache *)
            return_some hash
        | None ->
            if Hashtbl.length table = 0 then
              (* The table is empty. We do not need to retrieve the hash
                 before retrieving the initial context, because we have
                 no reference hash yet. I mean that, if block is <head>,
                 we are about to retrieve the node's head, no matter its value.
                 Any value is fine and its hash is going to be put right away in
                 the cache (see the call to [B2H.add]).
                 This avoids one RPC call, but it is
                 important because it is the first one: often there is
                 a single call. Skipping it reduces the node's load.

                 In the heavyduty.py scenario (see the proxy's original MR:
                 !1943), it reduces the number of calls to RPC .../hash
                 from 1200 to 700.
              *)
              return_none
            else
              (* Table is not empty, We need to be consistent with the previous call
                 and we dont have the data available:
                 need to do an RPC call to get the hash *)
              S.hash rpc_context ~chain ~block () >>=? fun hash ->
              (* Fill cache with result *)
              Hashtbl.add table (chain, block) hash ;
              return_some hash)
end

type mode = Light of Light.sources | Proxy

let build_directory (printer : Tezos_client_base.Client_context.printer)
    (rpc_context : RPC_context.json) (mode : mode)
    (proxy_env : Registration.proxy_environment) : unit RPC_directory.t =
  let (module Proxy_environment) = proxy_env in
  let module B2H = BlockToHash (Proxy_environment) in
  let envs_cache = Hashtbl.create 17 in
  let make chain block (module P_RPC : Proxy_proto.PROTO_RPC) =
    match mode with
    | Light sources ->
        let (module C) =
          Light_core.get_core (module Proxy_environment) printer sources
        in
        let (chain_string, block_string) =
          Tezos_shell_services.Block_services.
            (chain_to_string chain, to_string block)
        in
        Light_logger.Logger.(emit core_created (chain_string, block_string))
        >>= fun () ->
        let module M = Proxy_getter.Make (C) (P_RPC) in
        Lwt.return (module M : Proxy_getter.M)
    | Proxy ->
        let module M = Proxy_getter.MakeProxy (P_RPC) in
        Lwt.return (module M : Proxy_getter.M)
  in
  let get_env_rpc_context chain block =
    B2H.hash_of_block rpc_context chain block >>=? fun block_hash_opt ->
    let (block_key, fill_b2h) =
      match block_hash_opt with
      | None ->
          ( block,
            fun (rpc_context : Tezos_protocol_environment.rpc_context) ->
              B2H.add chain block rpc_context.block_hash )
      | Some block_hash -> (`Hash (block_hash, 0), ignore)
    in
    let key = (chain, block_key) in
    match Hashtbl.find_opt envs_cache key with
    | None ->
        Proxy_environment.init_env_rpc_context
          printer
          (make chain block_key)
          rpc_context
          chain
          block_key
        >>=? fun rpc_context ->
        fill_b2h rpc_context ;
        Hashtbl.add envs_cache key rpc_context ;
        return rpc_context
    | Some cached -> return cached
  in
  let get_env_rpc_context' chain block =
    get_env_rpc_context chain block >>= fun result ->
    match result with
    | Ok x -> Lwt.return x
    | Error errs ->
        (* proto_directory expects a unit Directory.t Lwt.t,
           we can't give it a unit tzresult Directory.t Lwt.t, hence
           throwing an exception. It's handled in
           [Tezos_mockup_proxy.RPC_client]. This is not ideal, but
           it's better than asserting false. *)
        raise (Tezos_mockup_proxy.RPC_client.Rpc_dir_creation_failure errs)
  in
  let proto_directory =
    let ( // ) = RPC_directory.prefix in
    (* register protocol-specific RPCs *)
    Tezos_shell_services.Chain_services.path
    // (Tezos_shell_services.Block_services.path
       // (* The Tezos_protocol_environment.rpc_context values returned
             by init_env_rpc_context contain proxy_getter's RPC
             cache. We wanna keep it in between RPC calls, hence
             the use of get_env_rpc_context' to cache init_env_rpc_context
             values. *)
       RPC_directory.map
         (fun ((_, chain), block) -> get_env_rpc_context' chain block)
         Proxy_environment.directory)
  in
  RPC_directory.register_describe_directory_service
    proto_directory
    RPC_service.description_service
