(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

open Protocol.Alpha_context
open Protocol_client_context
open Common

module Tezos_blocks_cache =
  Ringo_lwt.Functors.Make_opt
    ((val Ringo.(
            map_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise))
       (Block_hash))

type rollup_info = Stores.rollup_info = {
  rollup_id : Tx_rollup.t;
  origination_level : int32 option;
}

type sync_levels = {processed_tezos_level : int32; known_tezos_level : int32}

type sync_info = {
  mutable synchronized : bool;
  on_synchronized : unit Lwt_condition.t;
  mutable current_levels : sync_levels;
  sync_level_input : sync_levels Lwt_watcher.input;
}

type t = {
  stores : Stores.t;
  cctxt : Protocol_client_context.full;
  context_index : Context.index;
  mutable head : L2block.t option;
  rollup_info : rollup_info;
  tezos_blocks_cache : Alpha_block_services.block_info Tezos_blocks_cache.t;
  constants : Constants.t;
  signers : Node_config.signers;
  caps : Node_config.caps;
  sync : sync_info;
}

(* Stands for the manager operation pass, in which the rollup transactions are
   stored. *)
let rollup_operation_index = 3

let get_head state = state.head

let fetch_tezos_block state hash =
  let open Lwt_syntax in
  let errors = ref [] in
  let find_in_cache hash fetch =
    let fetch hash =
      let* block = fetch hash in
      match block with
      | Error errs ->
          errors := errs ;
          return_none
      | Ok block -> return_some block
    in
    let+ block =
      Tezos_blocks_cache.find_or_replace state.tezos_blocks_cache hash fetch
    in
    Result.of_option ~error:!errors block
    |> record_trace (Error.Tx_rollup_cannot_fetch_tezos_block hash)
  in

  fetch_tezos_block ~find_in_cache state.cctxt hash

let set_tezos_head state new_head_hash =
  let open Lwt_result_syntax in
  let*! old_head_hash = Stores.Tezos_head_store.read state.stores.tezos_head in
  let* reorg =
    match old_head_hash with
    | None ->
        (* No known tezos head, consider the new head as being on top of a previous
           tezos block. *)
        let+ new_head = fetch_tezos_block state new_head_hash in
        {old_chain = []; new_chain = [new_head]}
    | Some old_head_hash ->
        tezos_reorg (fetch_tezos_block state) ~old_head_hash ~new_head_hash
  in
  let* () =
    Stores.Tezos_head_store.write state.stores.tezos_head new_head_hash
  in
  return reorg

let save_tezos_block_info state block l2_block ~level ~predecessor =
  Stores.Tezos_block_store.add
    state.stores.tezos_blocks
    block
    {Stores.Tezos_block_store.l2_block; level; predecessor}

let get_tezos_l2_block_hash state block =
  let open Lwt_syntax in
  let+ info = Stores.Tezos_block_store.find state.stores.tezos_blocks block in
  Option.bind info (fun i -> i.Stores.Tezos_block_store.l2_block)

let get_block_store stores hash =
  Stores.L2_block_store.read_block stores.Stores.blocks hash

let get_block state hash = get_block_store state.stores hash

let get_header state hash =
  let open Lwt_syntax in
  let+ block = get_block state hash in
  Option.map (fun b -> b.L2block.header) block

let save_block state block =
  Stores.L2_block_store.append_block state.stores.blocks block

let get_inbox state hash =
  let open Lwt_syntax in
  let+ block = get_block state hash in
  Option.map (fun b -> b.L2block.inbox) block

let get_tezos_l2_block state block =
  let open Lwt_syntax in
  let* l2_hash = get_tezos_l2_block_hash state block in
  match l2_hash with
  | None -> return None
  | Some l2_hash -> get_block state l2_hash

let get_level state level = Stores.Level_store.find state.stores.levels level

let save_level state level hash =
  Stores.Level_store.add state.stores.levels level hash

let get_level_l2_block_header state level =
  let open Lwt_syntax in
  let* l2_hash = get_level state level in
  match l2_hash with
  | None -> return None
  | Some l2_hash -> get_header state l2_hash

let get_level_l2_block state level =
  let open Lwt_syntax in
  let* l2_hash = get_level state level in
  match l2_hash with
  | None -> return None
  | Some l2_hash -> get_block state l2_hash

let save_block state (block : L2block.t) =
  let open Lwt_syntax in
  join [save_level state block.header.level block.hash; save_block state block]

let distance_l2_levels l1 l2 =
  Int32.sub (Tx_rollup_level.to_int32 l2) (Tx_rollup_level.to_int32 l1)

(* Compute the reorganization of L2 blocks from the chain whose head is
   [old_head_hash] and the chain whose head [new_head_hash]. *)
let rollup_reorg state ~old_head ~new_head =
  let open Lwt_syntax in
  let get_pred b =
    match b.L2block.header.predecessor with
    | None -> Lwt.return_none
    | Some b -> get_block state b
  in
  let rec loop old_chain new_chain old_head new_head =
    match (old_head, new_head) with
    | None, _ | _, None ->
        return {old_chain = List.rev old_chain; new_chain = List.rev new_chain}
    | Some old_head, Some new_head ->
        if L2block.Hash.(old_head.L2block.hash = new_head.L2block.hash) then
          return
            {old_chain = List.rev old_chain; new_chain = List.rev new_chain}
        else
          let diff =
            distance_l2_levels
              old_head.L2block.header.level
              new_head.L2block.header.level
          in
          let* old_chain, new_chain, old, new_ =
            if diff = 0l then
              (* Heads at same level *)
              let new_chain = new_head :: new_chain in
              let old_chain = old_head :: old_chain in
              let* new_head = get_pred new_head in
              let+ old_head = get_pred old_head in
              (old_chain, new_chain, old_head, new_head)
            else if diff > 0l then
              (* New chain is longer *)
              let new_chain = new_head :: new_chain in
              let+ new_head = get_pred new_head in
              (old_chain, new_chain, Some old_head, new_head)
            else
              (* Old chain was longer *)
              let old_chain = old_head :: old_chain in
              let+ old_head = get_pred old_head in
              (old_chain, new_chain, old_head, Some new_head)
          in
          loop old_chain new_chain old new_
  in
  loop [] [] (Some old_head) (Some new_head)

let patch_l2_levels state (reorg : L2block.t reorg) =
  let open Lwt_result_syntax in
  let*! () =
    List.iter_s
      (fun old ->
        Stores.Level_store.remove state.stores.levels old.L2block.header.level)
      reorg.old_chain
  in
  List.iter_s
    (fun new_ -> save_level state new_.L2block.header.level new_.L2block.hash)
    reorg.new_chain

let set_head state head =
  let open Lwt_result_syntax in
  state.head <- Some head ;
  let*! old_head = Stores.Head_store.read state.stores.head in
  let hash = head.L2block.hash in
  let* () = Stores.Head_store.write state.stores.head hash in
  let*! l2_reorg =
    match old_head with
    | None -> Lwt.return no_reorg
    | Some old_head_hash -> (
        let*! old_head = get_block state old_head_hash in
        match old_head with
        | None -> Lwt.return no_reorg
        | Some old_head -> rollup_reorg state ~old_head ~new_head:head)
  in
  let*! () = patch_l2_levels state l2_reorg in
  return l2_reorg

let tezos_block_already_processed state block =
  let open Lwt_syntax in
  let* info = Stores.Tezos_block_store.find state.stores.tezos_blocks block in
  match info with
  | None -> return `Unknown
  | Some {l2_block = None; _} -> return (`Known None)
  | Some {l2_block = Some l2_hash; _} ->
      let+ block = get_block state l2_hash in
      `Known block

let get_included_commitment state commitment_hash =
  let open Lwt_syntax in
  let+ info =
    Stores.Commitment_store.find state.stores.commitments commitment_hash
  in
  Option.map
    (fun Stores.Commitment_store.{block; operation} ->
      L2block.{block; operation})
    info

let set_commitment_included state commitment_hash block operation =
  Stores.Commitment_store.add
    state.stores.commitments
    commitment_hash
    Stores.Commitment_store.{block; operation}

let unset_commitment_included state commitment_hash =
  Stores.Commitment_store.remove state.stores.commitments commitment_hash

let get_finalized_level state =
  Stores.Finalized_level_store.read state.stores.finalized_level

let set_finalized_level state l =
  Stores.Finalized_level_store.write state.stores.finalized_level l

let delete_finalized_level state =
  Stores.Finalized_level_store.delete state.stores.finalized_level

let get_block_metadata state (header : L2block.header) =
  let open Lwt_syntax in
  let* commitment_included = get_included_commitment state header.commitment in
  let+ finalized_level = get_finalized_level state in
  let finalized =
    match finalized_level with
    | None -> false
    | Some l -> Tx_rollup_level.(header.level >= l)
  in
  L2block.{commitment_included; finalized}

let get_block_and_metadata state hash =
  let open Lwt_syntax in
  let* block = get_block state hash in
  match block with
  | None -> return_none
  | Some block ->
      let* metadata = get_block_metadata state block.header in
      return_some (block, metadata)

let set_rollup_info state rollup_id ~origination_level =
  let rollup_info = {rollup_id; origination_level = Some origination_level} in
  Stores.Rollup_info_store.write state.stores.Stores.rollup_info rollup_info

let init_rollup_info stores ?origination_level rollup_id =
  let open Lwt_result_syntax in
  let*! stored_info = Stores.Rollup_info_store.read stores.Stores.rollup_info in
  let* rollup_info =
    match stored_info with
    | Some stored when Tx_rollup.(stored.rollup_id <> rollup_id) ->
        fail [Error.Tx_rollup_mismatch]
    | Some stored -> return stored
    | None ->
        let rollup_info = {rollup_id; origination_level} in
        let* () =
          Stores.Rollup_info_store.write stores.rollup_info rollup_info
        in
        return rollup_info
  in
  return rollup_info

let init_context ~data_dir =
  let open Lwt_result_syntax in
  let*! index = Context.init (Node_data.context_dir data_dir) in
  return index

let read_head (stores : Stores.t) =
  let open Lwt_syntax in
  let* hash = Stores.Head_store.read stores.head in
  match hash with
  | None -> return_none
  | Some hash -> get_block_store stores hash

let retrieve_constants cctxt =
  Protocol.Constants_services.all cctxt (cctxt#chain, cctxt#block)

let init (cctxt : #Protocol_client_context.full) ?(readonly = false)
    configuration =
  let open Lwt_result_syntax in
  let {
    Node_config.data_dir;
    rollup_id;
    origination_level;
    signers;
    l2_blocks_cache_size;
    caps;
    _;
  } =
    configuration
  in
  let*! stores =
    Stores.init ~data_dir ~readonly ~blocks_cache_size:l2_blocks_cache_size
  in
  let* rollup_info, context_index =
    both
      (init_rollup_info stores ?origination_level rollup_id)
      (init_context ~data_dir)
    |> lwt_map_error (function [] -> [] | trace :: _ -> trace)
  in
  let*! head = read_head stores in
  let* constants = retrieve_constants cctxt in
  (* L1 blocks are cached to handle reorganizations efficiently *)
  let tezos_blocks_cache = Tezos_blocks_cache.create 32 in
  let sync =
    {
      synchronized = false;
      on_synchronized = Lwt_condition.create ();
      current_levels = {processed_tezos_level = 0l; known_tezos_level = 0l};
      sync_level_input = Lwt_watcher.create_input ();
    }
  in
  return
    {
      stores;
      cctxt = (cctxt :> Protocol_client_context.full);
      context_index;
      head;
      rollup_info;
      tezos_blocks_cache;
      constants;
      signers;
      caps;
      sync;
    }

let notify_processed_tezos_level state processed_tezos_level =
  state.sync.current_levels <-
    {state.sync.current_levels with processed_tezos_level} ;
  Lwt_watcher.notify state.sync.sync_level_input state.sync.current_levels

let set_known_tezos_level state known_tezos_level =
  state.sync.current_levels <-
    {state.sync.current_levels with known_tezos_level}

let synchronized state =
  if state.sync.synchronized then Lwt.return_unit
  else Lwt_condition.wait state.sync.on_synchronized
