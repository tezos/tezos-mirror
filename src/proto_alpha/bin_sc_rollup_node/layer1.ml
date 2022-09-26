(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Configuration
open Protocol.Alpha_context
open Plugin
open Injector_common

(**

    Errors
    ======

*)

type error += Cannot_find_block of Block_hash.t

let () =
  register_error_kind
    ~id:"sc_rollup.node.cannot_find_block"
    ~title:"Cannot find block from L1"
    ~description:"A block couldn't be found from the L1 node"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Block with hash %a was not found on the L1 node."
        Block_hash.pp
        hash)
    `Temporary
    Data_encoding.(obj1 (req "hash" Block_hash.encoding))
    (function Cannot_find_block hash -> Some hash | _ -> None)
    (fun hash -> Cannot_find_block hash)

type error += Cannot_find_predecessor of Block_hash.t

let () =
  register_error_kind
    ~id:"sc_rollup.node.cannot_find_predecessor"
    ~title:"Cannot find block predecessor from L1"
    ~description:"A predecessor couldn't be found from the L1 node"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Block with hash %a has no predecessor on the L1 node."
        Block_hash.pp
        hash)
    `Temporary
    Data_encoding.(obj1 (req "hash" Block_hash.encoding))
    (function Cannot_find_predecessor hash -> Some hash | _ -> None)
    (fun hash -> Cannot_find_predecessor hash)

(**

   State
   =====

*)

type head = {hash : Block_hash.t; level : int32}

let head_encoding =
  Data_encoding.(
    conv
      (fun {hash; level} -> (hash, level))
      (fun (hash, level) -> {hash; level})
      (obj2 (req "hash" Block_hash.encoding) (req "level" Data_encoding.int32)))

module Blocks_cache =
  Ringo_lwt.Functors.Make_opt
    ((val Ringo.(
            map_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise))
       (Block_hash))

type blocks_cache =
  Protocol_client_context.Alpha_block_services.block_info Blocks_cache.t

type t = {
  blocks_cache : blocks_cache;
  heads : head Lwt_stream.t;
  cctxt : Protocol_client_context.full;
  stopper : RPC_context.stopper;
  genesis_info : Sc_rollup.Commitment.genesis_info;
}

(**

   Helpers
   =======

*)

let genesis_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"

(** [predecessors_of_blocks hashes] given a list of successive hashes,
    returns an associative list that associates a hash to its
    predecessor in this list. *)
let predecessors_of_blocks hashes =
  let rec aux next = function [] -> [] | x :: xs -> (next, x) :: aux x xs in
  match hashes with [] -> [] | x :: xs -> aux x xs

(** [get_predecessor block_hash] returns the predecessor block hash of
    some [block_hash] through an RPC to the Tezos node. To limit the
    number of RPCs, this information is requested for a batch of hashes
    and cached locally. *)
let get_predecessor =
  let max_cached = 1023 and max_read = 8 in
  let (module HMF : Ringo.MAP_MAKER) =
    Ringo.(map_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise)
  in
  let module HM = HMF (Block_hash) in
  let cache = HM.create max_cached in
  fun cctxt (chain : Tezos_shell_services.Chain_services.chain) ancestor ->
    let open Lwt_result_syntax in
    match HM.find_opt cache ancestor with
    | Some pred -> return_some pred
    | None -> (
        let* blocks =
          Tezos_shell_services.Chain_services.Blocks.list
            cctxt
            ~chain
            ~heads:[ancestor]
            ~length:max_read
            ()
        in
        match blocks with
        | [ancestors] -> (
            List.iter
              (fun (h, p) -> HM.replace cache h p)
              (predecessors_of_blocks ancestors) ;
            match HM.find_opt cache ancestor with
            | None ->
                (* We have just updated the cache with that information. *)
                assert false
            | Some predecessor -> return_some predecessor)
        | _ -> return_none)

let get_predecessor_opt state {level; hash} =
  let open Lwt_result_syntax in
  if level = 0l then return_none
  else
    let level = Int32.pred level in
    let+ hash = get_predecessor state.cctxt state.cctxt#chain hash in
    Option.map (fun hash -> {level; hash}) hash

let get_predecessor state ({hash; _} as head) =
  let open Lwt_result_syntax in
  let* pred = get_predecessor_opt state head in
  match pred with
  | None -> tzfail (Cannot_find_predecessor hash)
  | Some pred -> return pred

let rec connect ?(count = 0) ~delay cctxt genesis_info store =
  let open Lwt_syntax in
  let* () =
    if count = 0 then return_unit
    else
      let fcount = float_of_int (count - 1) in
      (* Randomized exponential backoff capped to 1.5h: 1.5^count * delay ± 50% *)
      let delay = delay *. (1.5 ** fcount) in
      let delay = min delay 3600. in
      let randomization_factor = 0.5 (* 50% *) in
      let delay =
        delay
        +. Random.float (delay *. 2. *. randomization_factor)
        -. (delay *. randomization_factor)
      in
      let* () = Event.wait_reconnect delay in
      Lwt_unix.sleep delay
  in
  let* res = Tezos_shell_services.Monitor_services.heads cctxt cctxt#chain in
  match res with
  | Ok (heads, stopper) ->
      let heads =
        Lwt_stream.map_s
          (fun (hash, Tezos_base.Block_header.{shell = {level; _}; _}) ->
            let+ () = Layer1_event.switched_new_head hash level in
            {hash; level})
          heads
      in
      return_ok (heads, stopper)
  | Error e ->
      let* () = Event.cannot_connect ~count e in
      connect ~delay ~count:(count + 1) cctxt genesis_info store

let start configuration (cctxt : Protocol_client_context.full) store =
  let open Lwt_result_syntax in
  let*! () = Layer1_event.starting () in
  let* kind =
    RPC.Sc_rollup.kind
      cctxt
      (cctxt#chain, cctxt#block)
      configuration.sc_rollup_address
      ()
  in
  let*! () = Event.rollup_exists ~addr:configuration.sc_rollup_address ~kind in
  let* genesis_info =
    RPC.Sc_rollup.genesis_info
      cctxt
      (cctxt#chain, cctxt#block)
      configuration.sc_rollup_address
  in
  let+ heads, stopper =
    connect ~delay:configuration.reconnection_delay cctxt genesis_info store
  in
  ( {cctxt; heads; blocks_cache = Blocks_cache.create 32; stopper; genesis_info},
    kind )

let reconnect configuration l1_ctxt store =
  let open Lwt_result_syntax in
  let* heads, stopper =
    connect
      ~count:1
      ~delay:configuration.reconnection_delay
      l1_ctxt.cctxt
      l1_ctxt.genesis_info
      store
  in
  return {l1_ctxt with heads; stopper}

let level_of_hash =
  let max_cached = 1023 in
  let cache = Blocks_cache.create max_cached in
  fun l1_ctxt hash ->
    let open Lwt_result_syntax in
    let*! level =
      match Blocks_cache.find_opt cache hash with
      | Some level -> level
      | None -> Lwt.return_none
    in
    match level with
    | Some level -> return level
    | None ->
        let+ {level; _} =
          Tezos_shell_services.Chain_services.Blocks.Header.shell_header
            l1_ctxt.cctxt
            ~block:(`Hash (hash, 0))
            ()
        in
        Blocks_cache.replace cache hash (Lwt.return_some level) ;
        level

let shutdown state =
  state.stopper () ;
  Lwt.return_unit

(** [fetch_tezos_block l1_ctxt hash] returns a block info given a block
    hash. Looks for the block in the blocks cache first, and fetches it from the
    L1 node otherwise. *)
let fetch_tezos_block l1_ctxt hash =
  trace (Cannot_find_block hash)
  @@ fetch_tezos_block
       l1_ctxt.cctxt
       hash
       ~find_in_cache:(Blocks_cache.find_or_replace l1_ctxt.blocks_cache)

(** Returns the reorganization of L1 blocks (if any) for [new_head]. *)
let get_tezos_reorg_for_new_head l1_state old_head_hash new_head_hash =
  let open Lwt_result_syntax in
  match old_head_hash with
  | None ->
      (* No known tezos head, consider the new head as being on top of a previous
         tezos block. *)
      let+ new_head = fetch_tezos_block l1_state new_head_hash in
      {old_chain = []; new_chain = [new_head]}
  | Some old_head_hash ->
      tezos_reorg (fetch_tezos_block l1_state) ~old_head_hash ~new_head_hash
