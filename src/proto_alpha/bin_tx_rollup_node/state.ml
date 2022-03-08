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
module Block_hash_map = Map.Make (Block_hash)

type rollup_origination = {block_hash : Block_hash.t; block_level : int32}

type t = {
  store : Stores.t;
  context_index : Context.index;
  rollup : Tx_rollup.t;
  rollup_origination : rollup_origination;
}

(* Stands for the manager operation pass, in which the rollup transactions are
   stored. *)
let rollup_operation_index = 3

let get_head state = Stores.L2_head.find state.store

let set_head state header = Stores.L2_head.set state.store header

let save_tezos_l2_block_hash state block info =
  Stores.Tezos_blocks.add state.store block info

let get_tezos_l2_block_hash state block =
  Stores.Tezos_blocks.find state.store block

let get_header state hash = Stores.L2_blocks.find state.store hash

let save_header state hash header = Stores.L2_blocks.add state.store hash header

let get_inbox state hash = Stores.Inboxes.find state.store hash

let save_inbox state hash inbox = Stores.Inboxes.add state.store hash inbox

let get_block state hash =
  let open Lwt_syntax in
  let* header = get_header state hash and* inbox = get_inbox state hash in
  match (header, inbox) with
  | (None, _) | (_, None) -> return None
  | (Some header, Some inbox) -> return (Some L2block.{header; inbox})

let get_tezos_l2_block state block =
  let open Lwt_syntax in
  let* l2_hash = get_tezos_l2_block_hash state block in
  match l2_hash with
  | None -> return None
  | Some l2_hash -> get_header state l2_hash

let get_level state level = Stores.Rollup_levels.find state.store level

let save_level state level hash =
  Stores.Rollup_levels.add state.store level hash

let get_level_l2_block state level =
  let open Lwt_syntax in
  let* l2_hash = get_level state level in
  match l2_hash with
  | None -> return None
  | Some l2_hash -> get_header state l2_hash

let save_block state L2block.{header; inbox} =
  let open Lwt_result_syntax in
  let hash = L2block.hash_header header in
  let+ () =
    join
      [
        save_tezos_l2_block_hash state header.tezos_block hash;
        save_level state header.level hash;
        save_header state hash header;
        save_inbox state hash inbox;
      ]
    |> lwt_map_error (function [] -> [] | trace :: _ -> trace)
  in
  hash

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
    | Some (Operation_metadata {contents}) ->
        check_origination_content_result_list contents
    | Some No_operation_metadata | None -> false
  in
  match Option.bind managed_operation @@ List.find_opt check_receipt with
  | Some _ -> return_unit
  | None -> fail @@ Error.Tx_rollup_not_originated_in_the_given_block rollup

let init_store ~data_dir ~context ?rollup_genesis rollup =
  let open Lwt_result_syntax in
  let* store = Stores.load (Node_data.store_dir data_dir) in
  let*! origination_info = Stores.Rollup_origination.find store in
  let store_rollup_origination =
    Option.map
      (fun (block_hash, block_level) -> {block_hash; block_level})
      origination_info
  in
  let* rollup_origination =
    match (store_rollup_origination, rollup_genesis) with
    | (None, None) ->
        fail
          [
            Error
            .Tx_rollup_no_rollup_origination_on_disk_and_no_rollup_genesis_given;
          ]
    | (Some {block_hash; _}, Some genesis)
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
    | (Some rollup_orig, _) -> return rollup_orig
    | (None, Some rollup_genesis) ->
        let block = `Hash (rollup_genesis, 0) in
        let* block_info =
          Alpha_block_services.info context ~chain:context#chain ~block ()
        in
        let* () = check_origination_in_block_info rollup block_info in
        let rollup_orig =
          {
            block_hash = rollup_genesis;
            block_level = block_info.header.shell.level;
          }
        in
        let* () =
          Stores.Rollup_origination.set
            store
            (rollup_orig.block_hash, rollup_orig.block_level)
        in
        return rollup_orig
  in
  return (store, rollup_origination)

let init_context ~data_dir =
  let open Lwt_result_syntax in
  let*! index = Context.init (Node_data.context_dir data_dir) in
  return index

let init ~data_dir ~context ?rollup_genesis rollup =
  let open Lwt_result_syntax in
  let store_orig = init_store ~data_dir ~context ?rollup_genesis rollup in
  let context_index = init_context ~data_dir in
  let* (store, rollup_origination) = store_orig in
  let* context_index = context_index in
  return {store; context_index; rollup; rollup_origination}
