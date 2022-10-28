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

exception Rpc_dir_creation_failure of tztrace

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

module BlockToHashClient : BLOCK_TO_HASH = struct
  let table = Hashtbl.create 17

  let add chain block hash = Hashtbl.add table (chain, block) hash

  let hash_of_block (rpc_context : #RPC_context.simple)
      (chain : Tezos_shell_services.Shell_services.chain)
      (block : Tezos_shell_services.Block_services.block) =
    let open Lwt_result_syntax in
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
          let* hash =
            Tezos_shell_services.Block_services.Empty.hash
              rpc_context
              ~chain
              ~block
              ()
          in
          (* Fill cache with result *)
          Hashtbl.add table (chain, block) hash ;
          return_some hash
end

type mode =
  | Light_client of Light.sources
  | Proxy_client
  | Proxy_server of {
      sleep : float -> unit Lwt.t;
      sym_block_caching_time : Ptime.span option;
      on_disk_proxy_builder :
        (Context_hash.t ->
        Tezos_protocol_environment.Proxy_delegate.t tzresult Lwt.t)
        option;
    }

let to_client_server_mode = function
  | Light_client _ | Proxy_client -> Proxy.Client
  | Proxy_server _ -> Proxy.Server

module BlockToHashServer : BLOCK_TO_HASH = struct
  let hash_of_block __ _ =
    let open Lwt_result_syntax in
    function
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
  match (mode, block) with
  | Light_client _, _ | Proxy_client, _ ->
      (* If octez-client executes: don't clear anything, because the
         client is short-lived and should not observe chain
         reorganization *)
      Lwt.return_unit
  | _, `Hash (_, n) when n >= 0 ->
      (* If block is identified by a hash in its future, don't clear
         anything as it doesn't deprecate.  Remember that contexts are
         kept in an LRU cache though, so clearing will eventually
         happen; but we don't schedule it. *)
      Lwt.return_unit
  | Proxy_server {sleep; sym_block_caching_time; _}, _ ->
      let chain_string, block_string =
        Tezos_shell_services.Block_services.
          (chain_to_string chain, to_string block)
      in
      let* time_between_blocks =
        match sym_block_caching_time with
        | Some sym_block_caching_time ->
            Lwt.return @@ Ptime.Span.to_float_s sym_block_caching_time
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
        let* () = sleep time_between_blocks in
        Env_cache_lwt.remove envs_cache key ;
        Events.(emit clearing_data (chain_string, block_string))
      in
      Lwt.dont_wait schedule (fun exc -> ignore exc) ;
      Lwt.return_unit

let build_directory (printer : Tezos_client_base.Client_context.printer)
    (rpc_context : RPC_context.generic) (mode : mode) force_protocol :
    unit RPC_directory.t =
  let b2h : (module BLOCK_TO_HASH) =
    match mode with
    | Proxy_server _ -> (module BlockToHashServer)
    | Light_client _ | Proxy_client -> (module BlockToHashClient)
  in
  let module B2H = (val b2h : BLOCK_TO_HASH) in
  let make proxy_env chain block =
    match mode with
    | Light_client sources ->
        Proxy_getter.Of_rpc
          (fun (module P_RPC : Proxy_proto.PROTO_RPC) ->
            let open Lwt_syntax in
            let (module C) = Light_core.get_core proxy_env printer sources in
            let chain_string, block_string =
              Tezos_shell_services.Block_services.
                (chain_to_string chain, to_string block)
            in
            let* () =
              Light_logger.Logger.(
                emit core_created (chain_string, block_string))
            in
            let module M = Proxy_getter.Make (C) (P_RPC) in
            Lwt.return (module M : Proxy_getter.M))
    | Proxy_client | Proxy_server {on_disk_proxy_builder = None; _} ->
        Proxy_getter.Of_rpc
          (fun (module P_RPC : Proxy_proto.PROTO_RPC) ->
            let module M = Proxy_getter.MakeProxy (P_RPC) in
            Lwt.return (module M : Proxy_getter.M))
    | Proxy_server {on_disk_proxy_builder = Some f; _} ->
        Proxy_getter.Of_data_dir f
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
    let open Lwt_result_syntax in
    let* block_hash_opt = B2H.hash_of_block rpc_context chain block in
    let block_key, (fill_b2h : Block_hash.t -> unit) =
      match block_hash_opt with
      | None -> (block, fun block_hash -> B2H.add chain block block_hash)
      | Some block_hash -> (`Hash (block_hash, 0), ignore)
    in
    let key = (chain, block_key) in
    let compute_value (chain, block_key) =
      let* proxy_env =
        Registration.get_registered_proxy
          printer
          rpc_context
          (match mode with Light_client _ -> `Mode_light | _ -> `Mode_proxy)
          ~chain
          ~block
          force_protocol
      in
      let (module Proxy_environment) = proxy_env in
      let ctx : Proxy_getter.rpc_context_args =
        {
          printer = Some printer;
          proxy_builder = make (module Proxy_environment) chain block_key;
          rpc_context;
          mode = to_client_server_mode mode;
          chain;
          block;
        }
      in
      let* initial_context = Proxy_environment.init_env_rpc_context ctx in
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
      let mapped_directory =
        RPC_directory.map
          (fun (_chain, _block) -> Lwt.return initial_context)
          Proxy_environment.directory
      in
      return
        (RPC_directory.register
           mapped_directory
           Tezos_shell_services.Block_services.Empty.S.protocols
           (fun _ctxt () () ->
             return
               Tezos_shell_services.Block_services.
                 {
                   current_protocol =
                     (* wrong if the block is a transition block! *)
                     Proxy_environment.protocol_hash;
                   next_protocol = Proxy_environment.protocol_hash;
                 }))
    in
    Env_cache_lwt.find_or_replace envs_cache key compute_value
  in
  let get_env_rpc_context' chain block =
    let open Lwt_syntax in
    let* res = get_env_rpc_context chain block in
    match res with
    | Error trace -> (
        (* proto_directory expects a unit Directory.t Lwt.t, and we
           can't give it a unit tzresult Directory.t Lwt.t, hence
           we throw an exception instead if we can't make the
           directory.

           This happens notably in the proxy server case when a
           request is made for a block baked on a protocol differing
           from the protocol the proxy server is currently running on.

           In the proxy server case, we'd prefer to return a 404
           instead of a 500. Luckily, Resto handles the [Not_found]
           exception specially and returns a 404, which our
           query-forwarding middleware (see
           Tezos_rpc_http.RPC_middleware) can then turn into a redirect
           to the node.

           In the client cases, we throw an exception (which Resto
           turns into a 500) and print the trace. *)
        match mode with
        | Proxy_server _ -> raise Not_found
        | Light_client _ | Proxy_client ->
            let* () =
              printer#warning
                "Error while building RPC directory (perhaps a protocol \
                 version mismatch between block and client?): %a"
                Error_monad.pp_print_trace
                trace
            in
            raise (Rpc_dir_creation_failure trace))
    | Ok res -> Lwt.return res
  in
  let proto_directory =
    (* register protocol-specific RPCs *)
    RPC_directory.register_dynamic_directory
      RPC_directory.empty
      (Tezos_rpc.RPC_path.prefix
         Tezos_shell_services.Chain_services.path
         Tezos_shell_services.Block_services.path)
      (fun ((_, chain), block) ->
        (* The Tezos_protocol_environment.rpc_context values returned
           by init_env_rpc_context contain proxy_getter's RPC
           cache. We wanna keep it in between RPC calls, hence
           the use of get_env_rpc_context' to cache init_env_rpc_context
           values. *)
        get_env_rpc_context' chain block)
  in
  RPC_directory.register_describe_directory_service
    proto_directory
    RPC_service.description_service
