(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol
open Alpha_context

(** If a slot, published at some level L, is expected to be confirmed at level
    L+D then, once the confirmation level is over, the rollup node is supposed to:
    - Download and save the content of the slot's pages in the store, if the slot
      is confirmed and the rollup is subscribed to it;
    - Add entries [None] for the slot's pages in the store, if the slot
      is not confirmed and the rollup is subscribed to it;
    - Do nothing, if the rollup node is not subscribed to that slot. *)

type error += Dal_slot_not_found_in_store of Dal.Slot.Header.id

let () =
  register_error_kind
    `Temporary
    ~id:"dal_pages_request.dal_slot_not_found_in_store"
    ~title:"Dal slot not found in store"
    ~description:"The Dal slot whose ID is given is not found in the store"
    ~pp:(fun ppf ->
      Format.fprintf ppf "Dal slot not found in store %a" Dal.Slot.Header.pp_id)
    Data_encoding.(obj1 (req "slot_id" Dal.Slot.Header.id_encoding))
    (function Dal_slot_not_found_in_store slot_id -> Some slot_id | _ -> None)
    (fun slot_id -> Dal_slot_not_found_in_store slot_id)

let store_entry_from_published_level ~dal_endorsement_lag ~published_level store
    =
  State.hash_of_level store
  @@ Int32.(
       add (of_int dal_endorsement_lag) (Raw_level.to_int32 published_level))

let slot_pages ~dal_endorsement_lag store
    (Dal.Slot.Header.{published_level; index} as slot_id) =
  let open Lwt_result_syntax in
  let*! pages =
    let*! confirmed_in_block_hash =
      store_entry_from_published_level
        ~dal_endorsement_lag
        ~published_level
        store
    in
    Store.Dal_slot_pages.list_secondary_keys_with_values
      store
      ~primary_key:confirmed_in_block_hash
  in
  let pages =
    List.filter
      (fun ((slot_idx, _page_idx), _v) -> Dal.Slot_index.equal index slot_idx)
      pages
  in
  let* () =
    fail_when (List.is_empty pages) (Dal_slot_not_found_in_store slot_id)
  in
  (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/4033
     This could be simplified with a simpler interface to store data.
     Otherwise, we should ideally check that we only have [Some <data>] or only
     [None] in the list. (i.e., all the pages of the slot are confirmed, or none
     is confirmed). *)
  List.fold_left
    (fun acc (_, v) -> match v with None -> acc | Some v -> v :: acc)
    []
    (List.rev pages)
  |> return

let page_content ~dal_endorsement_lag store page_id =
  let open Lwt_result_syntax in
  let Dal.Page.{slot_id; page_index} = page_id in
  let Dal.Slot.Header.{published_level; index} = slot_id in
  let*! confirmed_in_block_hash =
    store_entry_from_published_level ~dal_endorsement_lag ~published_level store
  in
  Store.Dal_slot_pages.find
    store
    ~primary_key:confirmed_in_block_hash
    ~secondary_key:(index, page_index)
  >|= Option.fold
        ~some:(fun v -> Ok v)
        ~none:(error (Dal_slot_not_found_in_store slot_id))
