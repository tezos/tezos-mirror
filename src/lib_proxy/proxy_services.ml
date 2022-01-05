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

module Directory = Resto_directory.Make (RPC_encoding)

module Events = struct
  include Internal_event.Simple

  let section = ["proxy_services"]

  let clearing_data =
    declare_2
      ~section
      ~name:"clearing_data"
      ~msg:"clearing data for chain {chain} and block {block}"
      ~level:Info
      ("chain", Data_encoding.string)
      ("block", Data_encoding.string)
end

module type BLOCK_TO_HASH = sig
  (** A [None] result value means the caller should use the block passed
      as third argument, even if not identified by a hash *)
  val hash_of_block :
    #RPC_context.simple ->
    Tezos_shell_services.Shell_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Block_hash.t option tzresult Lwt.t

  (** [add chain block hash] records that [block] (which
      may be symbolic, like [head]) has the given [hash] *)
  val add :
    Tezos_shell_services.Shell_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Block_hash.t ->
    unit
end

module Hashtbl = Stdlib.Hashtbl

let raw_hash_of_block (block : Tezos_shell_services.Block_services.block) :
    Block_hash.t option =
  match block with
  | `Hash (h, 0) -> Some h
  | `Alias (_, _) | `Genesis | `Head _ | `Level _ | `Hash (_, _) -> None

module BlockToHashClient (S : Registration.Proxy_sig) : BLOCK_TO_HASH = struct
  let table = Hashtbl.create 17

  let add chain block hash = Hashtbl.add table (chain, block) hash

  let hash_of_block (rpc_context : #RPC_context.simple)
      (chain : Tezos_shell_services.Shell_services.chain)
      (block : Tezos_shell_services.Block_services.block) =
    let open Lwt_tzresult_syntax in
    match raw_hash_of_block block with
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
              let* hash = S.hash rpc_context ~chain ~block () in
              (* Fill cache with result *)
              Hashtbl.add table (chain, block) hash ;
              return_some hash)
end

type mode =
  | Light_client of Light.sources
  | Proxy_client
  | Proxy_server of int option

let to_client_server_mode = function
  | Light_client _ | Proxy_client -> Proxy.Client
  | Proxy_server _ -> Proxy.Server

module BlockToHashServer : BLOCK_TO_HASH = struct
  let hash_of_block __ _ = function
    | `Hash (h, 0) -> return_some h
    | `Alias (_, _) | `Genesis | `Head _ | `Level _ | `Hash (_, _) ->
        return_none

  let add _ _ _ = ()
end

type env_cache_key =
  Tezos_shell_services.Chain_services.chain
  * Tezos_shell_services.Block_services.block

module Env_cache_key_hashed_type :
  Hashtbl.HashedType with type t = env_cache_key = struct
  type t = env_cache_key

  let equal ((lchain, lblock) : t) ((rchain, rblock) : t) =
    (* Avoid using polymorphic equality *)
    lchain = rchain && lblock = rblock

  let hash = Hashtbl.hash
end

module Env_cache =
  (* Rationale for this configuration:

     - Using LRU as it'll discard old heads if the client is always using
       the <head> identifier (supposing that new blocks keep coming)
     - overflow:Strong: we want collection to happen before the GC MUST do
       it, because we don't want performance to degrade with a nearly-full
       heap all the time.
     - accounting:Sloppy: because ringo specifies that Sloppy's antagonist
       (Precise) should mainly be used when removing a lot or inserting the same
       key often. We do none of that, so Sloppy seems better. *)
    (val Ringo.(map_maker ~replacement:LRU ~overflow:Strong ~accounting:Sloppy))
    (Env_cache_key_hashed_type)

module Env_cache_lwt = Ringo_lwt.Functors.Make_result (Env_cache)

let schedule_clearing (printer : Tezos_client_base.Client_context.printer)
    (rpc_context : RPC_context.generic)
    (proxy_env : Registration.proxy_environment) (mode : mode) envs_cache key
    chain block =
  let open Lwt_syntax in
  match (mode, raw_hash_of_block block) with
  | (Light_client _, _) | (Proxy_client, _) | (_, Some _) ->
      (* - If tezos-client executes: don't clear anything, because the client
           is short-lived and should not observe chain reorganization
         - If raw_hash_of_blocks returns [Some]: don't clear anything, because
           block is identified by its hash, hence it doesn't deprecate.
           Remember that contexts are kept in an LRU cache though, so clearing
           will eventually happen; but we don't schedule it. *)
      Lwt.return_unit
  | (Proxy_server sym_block_caching_time_opt, _) ->
      let (chain_string, block_string) =
        Tezos_shell_services.Block_services.
          (chain_to_string chain, to_string block)
      in
      let* time_between_blocks =
        match sym_block_caching_time_opt with
        | Some sym_block_caching_time ->
            Lwt.return @@ Int.to_float sym_block_caching_time
        | None -> (
            let (module Proxy_environment) = proxy_env in
            let* ro =
              Proxy_environment.time_between_blocks rpc_context chain block
            in
            match ro with
            | Error _ | Ok None ->
                (* While this looks like hardcoding an important value, it's not.
                   This block is entered if and only if: 1/ the RPC retrieving
                   the constants fail (see [Proxy_environment.time_between_blocks]
                   implementation that relies on this RPC). Or 2/ the
                   protocol doesn't specify the constant time_between_blocks,
                   which ought to be impossible. *)
                let* () =
                  printer#warning
                    "time_between_blocks for chain %s and block %s cannot be \
                     determined. Using 60 seconds."
                    chain_string
                    block_string
                in
                Lwt.return 60.0
            | Ok (Some x) -> Lwt.return (Int64.to_float x))
      in
      let schedule () : _ Lwt.t =
        let* () = Lwt_unix.sleep time_between_blocks in
        Env_cache_lwt.remove envs_cache key ;
        Events.(emit clearing_data (chain_string, block_string))
      in
      Lwt.dont_wait schedule (fun exc -> ignore exc) ;
      Lwt.return_unit

(** [protocols hash] returns the implementation of the RPC
    [/chains/<chain_id>/blocks/<block_id>/protocols] of the proxy server.

    It is conservative: it always return the protocol that was
    returned by the node for its [head] block when [tezos-proxy-server]
    started. While it look like we could do something smarter in
    [build_directory] (such as inspecting the header of the block being queried),
    it is impossible to have a different implementation than this one.
    That's because the proxy server will anyway only succeed on blocks that are
    in the protocol returned initially by the node, as the
    proxy server's implementation is protocol-dependent (see [proxy_env]
    in [build_directory]).

    That is why users of proxy servers need to be aware that proxy servers are
    imprecise near the time of a protocol change and that proxy servers
    should be eventually discarded and restarted. *)
let protocols protocol_hash =
  let open Tezos_shell_services in
  let path =
    let open Tezos_rpc.RPC_path in
    prefix Block_services.chain_path Block_services.path
  in
  let service =
    Tezos_rpc.RPC_service.prefix path Block_services.Empty.S.protocols
  in
  Directory.register Directory.empty service (fun _prefix () () ->
      Lwt.return
        (`Ok
          {
            Block_services.current_protocol = protocol_hash;
            next_protocol = protocol_hash;
          }))

let build_directory (printer : Tezos_client_base.Client_context.printer)
    (rpc_context : RPC_context.generic) (mode : mode)
    (proxy_env : Registration.proxy_environment) : unit RPC_directory.t =
  let (module Proxy_environment) = proxy_env in
  let b2h : (module BLOCK_TO_HASH) =
    match mode with
    | Proxy_server _ -> (module BlockToHashServer)
    | Light_client _ | Proxy_client ->
        (module BlockToHashClient (Proxy_environment))
  in
  let module B2H = (val b2h : BLOCK_TO_HASH) in
  let make chain block (module P_RPC : Proxy_proto.PROTO_RPC) =
    let open Lwt_syntax in
    match mode with
    | Light_client sources ->
        let (module C) =
          Light_core.get_core (module Proxy_environment) printer sources
        in
        let (chain_string, block_string) =
          Tezos_shell_services.Block_services.
            (chain_to_string chain, to_string block)
        in
        let* () =
          Light_logger.Logger.(emit core_created (chain_string, block_string))
        in
        let module M = Proxy_getter.Make (C) (P_RPC) in
        Lwt.return (module M : Proxy_getter.M)
    | Proxy_client | Proxy_server _ ->
        let module M = Proxy_getter.MakeProxy (P_RPC) in
        Lwt.return (module M : Proxy_getter.M)
  in
  (* proxy_server case: given that a new block arrives every minute,
     make the cache keep blocks from approximately the last hour.
     Starting at protocol G, blocks may arrive faster than one per minute.
     We can either forward the protocol's constants here, or do an
     RPC call to obtain the exact value.
     Anyway we're safe, having an appromixation here is fine. *)
  let envs_cache =
    Env_cache_lwt.create
      (match mode with
      | Proxy_server _ -> 64
      | Proxy_client | Light_client _ -> 16)
  in
  let get_env_rpc_context chain block =
    let open Lwt_tzresult_syntax in
    let* block_hash_opt = B2H.hash_of_block rpc_context chain block in
    let (block_key, (fill_b2h : Block_hash.t -> unit)) =
      match block_hash_opt with
      | None -> (block, fun block_hash -> B2H.add chain block block_hash)
      | Some block_hash -> (`Hash (block_hash, 0), ignore)
    in
    let key = (chain, block_key) in
    let compute_value (chain, block_key) =
      let* initial_context =
        Proxy_environment.init_env_rpc_context
          printer
          (make chain block_key)
          rpc_context
          (to_client_server_mode mode)
          chain
          block_key
      in
      fill_b2h @@ initial_context.block_hash ;
      let*! () =
        schedule_clearing
          printer
          rpc_context
          proxy_env
          mode
          envs_cache
          key
          chain
          block
      in
      return initial_context
    in
    Env_cache_lwt.find_or_replace envs_cache key compute_value
  in
  let get_env_rpc_context' chain block =
    let open Lwt_syntax in
    let* res = get_env_rpc_context chain block in
    match res with
    | Error trace ->
        (* proto_directory expects a unit Directory.t Lwt.t,
           we can't give it a unit tzresult Directory.t Lwt.t, hence
           throwing an exception. It's handled in
           [Tezos_mockup_proxy.RPC_client]. This is not ideal, but
           it's better than asserting false. *)
        raise (Tezos_mockup_proxy.RPC_client.Rpc_dir_creation_failure trace)
    | Ok res -> Lwt.return res
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
  let whole_directory =
    Directory.merge
      (match mode with
      | Proxy_server _ -> protocols Proxy_environment.protocol_hash
      | Light_client _ | Proxy_client -> Directory.empty)
      proto_directory
  in
  RPC_directory.register_describe_directory_service
    whole_directory
    RPC_service.description_service
