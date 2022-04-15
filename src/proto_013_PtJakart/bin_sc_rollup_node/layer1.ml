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

(**

    Errors
    ======

*)
let synchronization_failure e =
  Format.eprintf
    "Error during synchronization: @[%a@]"
    Error_monad.(TzTrace.pp_print_top pp)
    e ;
  Lwt_exit.exit_and_raise 1

(**

   State
   =====

*)

type block_hash = Block_hash.t

type block = Block of {predecessor : block_hash; level : int32}

let block_encoding =
  Data_encoding.(
    conv
      (fun (Block {predecessor; level}) -> (predecessor, level))
      (fun (predecessor, level) -> Block {predecessor; level})
      (obj2
         (req "predecessor" Block_hash.encoding)
         (req "level" Data_encoding.int32)))

type head = Head of {hash : block_hash; level : int32}

let head_encoding =
  Data_encoding.(
    conv
      (fun (Head {hash; level}) -> (hash, level))
      (fun (hash, level) -> Head {hash; level})
      (obj2 (req "hash" Block_hash.encoding) (req "level" Data_encoding.int32)))

module State = struct
  let reorganization_window_length = 10

  module Store = struct
    module Blocks = Store.Make_append_only_map (struct
      let path = ["tezos"; "blocks"]

      let keep_last_n_entries_in_memory = reorganization_window_length

      type key = block_hash

      let string_of_key = Block_hash.to_b58check

      type value = block

      let value_encoding = block_encoding
    end)

    module Head = Store.Make_mutable_value (struct
      let path = ["tezos"; "head"]

      type value = head

      let value_encoding = head_encoding
    end)
  end

  let already_seen = Store.Blocks.mem

  let last_seen_head = Store.Head.find

  let set_new_head = Store.Head.set

  let store_block = Store.Blocks.add

  let block_of_hash = Store.Blocks.get
end

(**

   Chain events
   ============

*)

type chain_event =
  | SameBranch of {new_head : head; intermediate_heads : head list}
  | Rollback of {new_head : head}

let same_branch new_head intermediate_heads =
  SameBranch {new_head; intermediate_heads}

let rollback new_head = Rollback {new_head}

(**

   Helpers
   =======

*)

let genesis_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2"

(** [blocks_of_heads base heads] given a list of successive heads
   connected to [base], returns an associative list mapping block hash
   to block. This list is only used for traversal, not lookup. The
   newer blocks come first in that list. *)
let blocks_of_heads base heads =
  let rec aux predecessor accu = function
    | [] -> accu
    | Head {hash; level} :: xs ->
        let block = Block {predecessor; level} in
        aux hash ((hash, block) :: accu) xs
  in
  aux base [] heads

(** [store_chain_event event] updates the persistent state to take a
    chain event into account. *)
let store_chain_event store base =
  let open Lwt_syntax in
  function
  | SameBranch {new_head = Head {hash; level} as head; intermediate_heads} ->
      let* () = Layer1_event.setting_new_head hash level in
      let* () = State.set_new_head store head in
      blocks_of_heads base (intermediate_heads @ [head])
      |> List.iter_s (fun (hash, block) -> State.store_block store hash block)
  | Rollback {new_head = Head {hash; level} as base} ->
      let* () = Layer1_event.rollback hash level in
      State.set_new_head store base

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
    match HM.find_opt cache ancestor with
    | Some pred -> Lwt.return (Some pred)
    | None -> (
        Tezos_shell_services.Chain_services.Blocks.list
          cctxt
          ~chain
          ~heads:[ancestor]
          ~length:max_read
          ()
        >>= function
        | Error e -> synchronization_failure e
        | Ok blocks -> (
            match blocks with
            | [ancestors] -> (
                List.iter
                  (fun (h, p) -> HM.replace cache h p)
                  (predecessors_of_blocks ancestors) ;
                match HM.find_opt cache ancestor with
                | None ->
                    (* We have just updated the cache with that information. *)
                    assert false
                | Some predecessor -> Lwt.return (Some predecessor))
            | _ -> Lwt.return None))

let get_predecessor_head cctxt chain (Head {level; hash}) =
  let open Lwt_syntax in
  let level = Int32.pred level in
  let+ hash' = get_predecessor cctxt chain hash in
  Option.map (fun hash' -> Head {level; hash = hash'}) hash'

(** [catch_up cctxt chain last_seen_head predecessor new_head]
   classifies the [new_head] (with some given [predecessor]) in two
   distinct categories:

   - If [new_head] has an ancestor which is the [last_seen_head],
   returns [SameBranch { new_head; intermediate_heads }] where
   [intermediate_heads] are the blocks between [last_seen_head] and
   [new_head] in order of increasing levels.

   - If [new_head] has an ancestor that is an ancestor [base] of
   [last_seen_head] then returns [Rollback { new_head }].

   This function also returns the block hash to which the current
   branch is rooted.
*)
let catch_up cctxt store chain last_seen_head new_head =
  let (Head {hash; _}) = last_seen_head in

  (* [heads] is the list of intermediate heads between
     the predecessor of [ancestor] and the [new_head]. [level]
     is the level of [ancestor]. *)
  let rec aux heads (Head {hash = ancestor_hash; level} as ancestor) =
    if Block_hash.equal ancestor_hash hash then
      (* We have reconnected to the last seen head. *)
      Lwt.return (ancestor_hash, [same_branch new_head heads])
    else
      State.already_seen store ancestor_hash >>= function
      | true ->
          (* We have reconnected to a previously known head.
             [new_head] and [last_seen_head] are not the same branch. *)
          Lwt.return
            (ancestor_hash, [rollback ancestor; same_branch new_head heads])
      | false -> (
          (* We have never seen this head. *)
          let heads = ancestor :: heads in
          get_predecessor cctxt chain ancestor_hash >>= function
          | Some ancestor' when Block_hash.(ancestor_hash <> ancestor') ->
              aux heads (Head {level = Int32.pred level; hash = ancestor'})
          | _ ->
              (* We have reconnected with the genesis head and it was
                 unknown until now. *)
              Lwt.return (ancestor_hash, [same_branch new_head heads]))
  in
  get_predecessor_head cctxt chain new_head >>= function
  | None ->
      (* [new_head] is the genesis head. It is not new. *)
      Lwt.return (genesis_hash, [])
  | Some predecessor -> aux [] predecessor

let chain_events cctxt store chain =
  let open Lwt_result_syntax in
  let on_head (hash, (block_header : Tezos_base.Block_header.t)) =
    let level = block_header.shell.level in
    let new_head = Head {hash; level} in
    let*! last_seen_head = State.last_seen_head store in
    let last_seen_head =
      match last_seen_head with
      | None -> Head {hash = genesis_hash; level = 0l}
      | Some last_seen_head -> last_seen_head
    in
    let*! (base, events) = catch_up cctxt store chain last_seen_head new_head in
    let*! () = List.iter_s (store_chain_event store base) events in
    Lwt.return events
  in
  let+ (heads, _) = Tezos_shell_services.Monitor_services.heads cctxt chain in
  Lwt_stream.map_list_s on_head heads

let check_sc_rollup_address_exists sc_rollup_address
    (cctxt : Protocol_client_context.full) =
  let open Lwt_result_syntax in
  let* kind_opt =
    RPC.Sc_rollup.kind cctxt (cctxt#chain, cctxt#block) sc_rollup_address ()
  in
  let*! () =
    match kind_opt with
    | None ->
        cctxt#error "%a does not exist" Sc_rollup.Address.pp sc_rollup_address
    | Some kind -> Event.rollup_exists ~addr:sc_rollup_address ~kind
  in
  return_unit

let start configuration (cctxt : Protocol_client_context.full) store =
  let open Lwt_result_syntax in
  let*! () = Layer1_event.starting () in
  let* () =
    check_sc_rollup_address_exists configuration.sc_rollup_address cctxt
  in
  chain_events cctxt store `Main

let current_head_hash store =
  let open Lwt_syntax in
  let+ head = State.last_seen_head store in
  Option.map (fun (Head {hash; _}) -> hash) head

let current_level store =
  let open Lwt_syntax in
  let+ head = State.last_seen_head store in
  Option.map (fun (Head {level; _}) -> level) head

let predecessor store (Head {hash; _}) =
  let open Lwt_syntax in
  let+ (Block {predecessor; _}) = State.block_of_hash store hash in
  predecessor

let processed_head (Head {hash; level}) =
  Layer1_event.new_head_processed hash level

let processed = function
  | SameBranch {new_head; intermediate_heads} ->
      List.iter_s processed_head (intermediate_heads @ [new_head])
  | Rollback {new_head} -> processed_head new_head
