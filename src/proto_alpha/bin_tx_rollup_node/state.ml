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
open Protocol.Apply_results
open Protocol_client_context
open Common

type rollup_origination = {block_hash : Block_hash.t; block_level : int32}

type t = {
  stores : Stores.t;
  context_index : Context.index;
  mutable head : L2block.t;
  rollup : Tx_rollup.t;
  rollup_origination : rollup_origination;
  parameters : Protocol.Tx_rollup_l2_apply.parameters;
  operator : signer option;
  batcher_state : Batcher.state option;
}

type 'block reorg = {
  ancestor : 'block option;
  old_chain : 'block list;
  new_chain : 'block list;
}

(* Stands for the manager operation pass, in which the rollup transactions are
   stored. *)
let rollup_operation_index = 3

let no_reorg = {ancestor = None; old_chain = []; new_chain = []}

let get_head state = state.head

let set_head state block =
  state.head <- block ;
  Stores.Head_store.write state.stores.head (L2block.hash_header block.header)

let save_tezos_block_info state block l2_block ~level ~predecessor =
  Stores.Tezos_block_store.add
    state.stores.tezos_blocks
    block
    {Stores.Tezos_block_store.l2_block; level; predecessor}

let get_tezos_l2_block_hash state block =
  let open Lwt_syntax in
  let+ info = Stores.Tezos_block_store.find state.stores.tezos_blocks block in
  Option.map (fun i -> i.Stores.Tezos_block_store.l2_block) info

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
  let hash = L2block.hash_header block.header in
  let+ () =
    join [save_level state block.header.level hash; save_block state block]
  in
  hash

let distance_l2_levels l1 l2 =
  let to_int32 = function
    | L2block.Genesis -> -1l
    | Rollup_level l -> Tx_rollup_level.to_int32 l
  in
  Int32.sub (to_int32 l2) (to_int32 l1)

(* Compute the reorganization of L2 blocks from the chain whose head is
   [old_head_hash] and the chain whose head [new_head_hash]. *)
let rollup_reorg state ~old_head ~new_head =
  let open Lwt_syntax in
  let rec loop old_chain new_chain (old_head, old_head_hash)
      (new_head, new_head_hash) =
    match (old_head, new_head) with
    | (None, _) | (_, None) ->
        return
          {
            ancestor = None;
            old_chain = List.rev old_chain;
            new_chain = List.rev new_chain;
          }
    | (Some old_head, Some new_head) ->
        if L2block.Hash.(old_head_hash = new_head_hash) then
          return
            {
              ancestor = Some (old_head, old_head_hash);
              old_chain = List.rev old_chain;
              new_chain = List.rev new_chain;
            }
        else
          let diff =
            distance_l2_levels
              old_head.L2block.header.level
              new_head.L2block.header.level
          in
          let* (old_chain, new_chain, old, new_) =
            if diff = 0l then
              (* Heads at same level *)
              let new_chain = (new_head, new_head_hash) :: new_chain in
              let old_chain = (old_head, old_head_hash) :: old_chain in
              let new_head_hash = new_head.L2block.header.predecessor in
              let old_head_hash = old_head.L2block.header.predecessor in
              let* new_head = get_block state new_head_hash in
              let+ old_head = get_block state old_head_hash in
              ( old_chain,
                new_chain,
                (old_head, old_head_hash),
                (new_head, new_head_hash) )
            else if diff > 0l then
              (* New chain is longer *)
              let new_chain = (new_head, new_head_hash) :: new_chain in
              let new_head_hash = new_head.L2block.header.predecessor in
              let+ new_head = get_block state new_head_hash in
              ( old_chain,
                new_chain,
                (Some old_head, old_head_hash),
                (new_head, new_head_hash) )
            else
              (* Old chain was longer *)
              let old_chain = (old_head, old_head_hash) :: old_chain in
              let old_head_hash = old_head.L2block.header.predecessor in
              let+ old_head = get_block state old_head_hash in
              ( old_chain,
                new_chain,
                (old_head, old_head_hash),
                (Some new_head, new_head_hash) )
          in
          loop old_chain new_chain old new_
  in
  let (old_head, old_head_hash) = old_head in
  let (new_head, new_head_hash) = new_head in
  loop [] [] (Some old_head, old_head_hash) (Some new_head, new_head_hash)

let patch_l2_levels state (reorg : (L2block.t * L2block.hash) reorg) =
  let open Lwt_result_syntax in
  let*! () =
    List.iter_s
      (fun (old_, _) ->
        Stores.Level_store.remove state.stores.levels old_.L2block.header.level)
      reorg.old_chain
  in
  List.iter_s
    (fun (new_, new_hash) ->
      save_level state new_.L2block.header.level new_hash)
    reorg.new_chain

let set_head state head context =
  let open Lwt_result_syntax in
  (match state.batcher_state with
  | None -> ()
  | Some batcher_state -> Batcher.update_incr_context batcher_state context) ;
  state.head <- head ;
  let*! old_head = Stores.Head_store.read state.stores.head in
  let* () =
    Stores.Head_store.write
      state.stores.head
      (L2block.hash_header head.L2block.header)
  in
  let hash = L2block.hash_header head.header in
  let*! l2_reorg =
    match old_head with
    | None -> Lwt.return no_reorg
    | Some old_head_hash -> (
        let*! old_head = get_block state old_head_hash in
        match old_head with
        | None -> Lwt.return no_reorg
        | Some old_head ->
            rollup_reorg
              state
              ~old_head:(old_head, old_head_hash)
              ~new_head:(head, hash))
  in
  let*! () = patch_l2_levels state l2_reorg in
  return l2_reorg

let tezos_block_already_processed state hash =
  let open Lwt_syntax in
  let+ info = get_tezos_l2_block_hash state hash in
  Option.is_some info

let check_origination_in_block_info rollup block_info =
  let extract_originated_tx_rollup :
      type kind. kind manager_operation_result -> Tx_rollup.t option = function
    | Applied (Tx_rollup_origination_result {originated_tx_rollup; _}) ->
        Some originated_tx_rollup
    | _ -> None
  in
  let check_origination_content_result : type kind. kind contents_result -> bool
      = function
    | Manager_operation_result {operation_result; _} ->
        operation_result |> extract_originated_tx_rollup
        |> Option.fold ~none:false ~some:(Tx_rollup.equal rollup)
    | _ -> false
  in
  let rec check_origination_content_result_list :
      type kind. kind contents_result_list -> bool = function
    | Single_result x -> check_origination_content_result x
    | Cons_result (x, xs) ->
        check_origination_content_result x
        || check_origination_content_result_list xs
  in
  let managed_operation =
    List.nth_opt
      block_info.Alpha_block_services.operations
      rollup_operation_index
  in
  let check_receipt operation =
    match operation.Alpha_block_services.receipt with
    | Receipt (Operation_metadata {contents}) ->
        check_origination_content_result_list contents
    | Receipt No_operation_metadata | Empty | Too_large -> false
  in
  match Option.bind managed_operation @@ List.find_opt check_receipt with
  | Some _ -> return_unit
  | None -> fail @@ Error.Tx_rollup_not_originated_in_the_given_block rollup

let init_rollup_origination cctxt stores ?rollup_genesis rollup =
  let open Lwt_result_syntax in
  let*! origination_info =
    Stores.Rollup_origination_store.read stores.Stores.rollup_origination
  in
  let* rollup_origination =
    match (origination_info, rollup_genesis) with
    | (None, None) ->
        fail
          [
            Error
            .Tx_rollup_no_rollup_origination_on_disk_and_no_rollup_genesis_given;
          ]
    | (Some (stored_rollup, _, _), __) when Tx_rollup.(stored_rollup <> rollup)
      ->
        fail [Error.Tx_rollup_mismatch]
    | (Some (_, block_hash, _), Some genesis)
      when Block_hash.(block_hash <> genesis) ->
        fail
          [
            Error
            .Tx_rollup_different_disk_stored_origination_rollup_and_given_rollup_genesis
              {
                disk_rollup_origination = block_hash;
                given_rollup_genesis = genesis;
              };
          ]
    | (Some (_, block_hash, block_level), _) -> return {block_hash; block_level}
    | (None, Some rollup_genesis) ->
        let block = `Hash (rollup_genesis, 0) in
        let* block_info =
          Alpha_block_services.info cctxt ~chain:cctxt#chain ~block ()
        in
        let* () = check_origination_in_block_info rollup block_info in
        let rollup_orig =
          {
            block_hash = rollup_genesis;
            block_level = block_info.header.shell.level;
          }
        in
        let* () =
          Stores.Rollup_origination_store.write
            stores.rollup_origination
            (rollup, rollup_orig.block_hash, rollup_orig.block_level)
        in
        return rollup_orig
  in
  return rollup_origination

let init_context ~data_dir =
  let open Lwt_result_syntax in
  let*! index = Context.init (Node_data.context_dir data_dir) in
  return index

let init_head (stores : Stores.t) context_index rollup rollup_origination =
  let open Lwt_syntax in
  let* hash = Stores.Head_store.read stores.head in
  let+ head =
    match hash with
    | None -> return_none
    | Some hash -> get_block_store stores hash
  in
  match head with
  | Some head -> head
  | None ->
      L2block.genesis_block context_index rollup rollup_origination.block_hash

let init_parameters cctxt =
  let open Lwt_result_syntax in
  let* {parametric; _} =
    Protocol.Constants_services.all cctxt (cctxt#chain, cctxt#block)
  in
  return
    {
      Protocol.Tx_rollup_l2_apply.tx_rollup_max_withdrawals_per_batch =
        parametric.tx_rollup_max_withdrawals_per_batch;
    }

let init cctxt ~data_dir ?(readonly = false) ?rollup_genesis ~operator rollup =
  let open Lwt_result_syntax in
  (* TODO/TORU make blocks_cache_size configurable *)
  let*! stores = Stores.init ~data_dir ~readonly ~blocks_cache_size:1024 in
  let* (rollup_origination, context_index) =
    both
      (init_rollup_origination cctxt stores ?rollup_genesis rollup)
      (init_context ~data_dir)
    |> lwt_map_error (function [] -> [] | trace :: _ -> trace)
  in
  let*! head = init_head stores context_index rollup rollup_info in
  let* parameters = init_parameters cctxt in
  let* batcher_state =
    Batcher.init cctxt ~rollup ~signer:operator context_index parameters
  in
  let* operator = get_signer cctxt operator in
  return
    {
      stores;
      context_index;
      head;
      rollup;
      rollup_origination;
      parameters;
      operator;
      batcher_state;
    }
