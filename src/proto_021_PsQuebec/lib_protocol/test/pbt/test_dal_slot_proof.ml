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

(** Testing
    -------
    Component:    PBT for refutation proofs of Dal
    Invocation:   dune exec src/proto_021_PtQenaB1/lib_protocol/test/pbt/main.exe \
                  -- --file test_dal_slot_proof.ml
    Subject:      Refutation proof-related functions of Dal
*)

open Protocol

module Make (Parameters : sig
  val name : string

  val count : int

  val dal_parameters : Alpha_context.Constants.Parametric.dal
end) =
struct
  module ARG = struct
    include Parameters

    let cryptobox =
      Lazy.from_fun @@ fun () ->
      Dal_helpers.mk_cryptobox Parameters.dal_parameters.cryptobox_parameters
  end

  open Dal_helpers.Make (ARG)

  (* Introduce some intermediate types. *)

  (** The slot is not confirmed (skipped) iff the boolean is [true]. *)
  type slot_skipped = bool

  type level = int

  type slots = level * slot_skipped list

  type levels = slots list

  (** Given a list of {!levels}, where each element is of type {!slots} = {!slot}
      list, and where each slot is a boolean, this function populates an
      empty slots_history skip list and a corresponding history_cache as follows:
      - the function starts from a given [start_level] (default is 1)
      - levels are incremented by 2 (to allow having levels without confirmed slots
      for test purpose).
      - every element in the list of levels represents the slots of a single level.
      - each slot of a given level is not confirmed iff the boolean is true. *)
  let populate_slots_history (levels_data : levels) =
    let open Lwt_result_wrap_syntax in
    (* Make and insert a slot. *)
    let slot_data =
      Bytes.init
        Parameters.(dal_parameters.cryptobox_parameters.slot_size)
        (fun _i -> 'x')
    in
    let* polynomial = dal_mk_polynomial_from_slot slot_data in
    let* cryptobox = Lazy.force ARG.cryptobox in
    let*? commitment = dal_commit cryptobox polynomial in
    (* Insert the slots of a level. *)
    let add_slots (cell, cache, slots_info) (level, slots_data) =
      let curr_level = Raw_level_repr.of_int32_exn (Int32.of_int level) in
      let slots_headers =
        List.mapi
          (fun sindex skip_slot ->
            let index =
              Option.value_f
                (Dal_slot_index_repr.of_int_opt
                   ~number_of_slots:Parameters.(dal_parameters.number_of_slots)
                   sindex)
                ~default:(fun () -> assert false)
            in
            ( Dal_slot_repr.Header.
                {id = {published_level = curr_level; index}; commitment},
              skip_slot ))
          slots_data
      in
      let attested_slots_headers =
        List.filter_map
          (fun (slot, skip_slot) -> if skip_slot then None else Some slot)
          slots_headers
      in
      let*?@ cell, cache =
        Dal_slot_repr.History.add_confirmed_slot_headers
          ~number_of_slots:Parameters.dal_parameters.number_of_slots
          cell
          cache
          curr_level
          attested_slots_headers
      in
      let slots_info =
        List.fold_left
          (fun slots_info (slot, skip_slot) ->
            (polynomial, slot, skip_slot) :: slots_info)
          slots_info
          slots_headers
      in
      return (cell, cache, slots_info)
    in

    (* Insert the slots of all the levels. *)
    let add_levels = List.fold_left_es add_slots in
    add_levels (genesis_history, genesis_history_cache, []) levels_data

  (** This function returns the (correct) information of a page to
      prove that it is confirmed, or None if the page's slot is skipped. *)
  let request_confirmed_page (poly, slot, skip_slot) =
    let open Lwt_result_syntax in
    if skip_slot then
      (* We cannot check that a page of an unconfirmed slot is confirmed. *)
      return_none
    else
      let* page_info, page_id = mk_page_info slot poly in
      return_some (page_info, page_id)

  (** This function returns information of a page to prove that it is
      unconfirmed, if the page's slot is skipped, the information look correct
      (but the slot is not confirmed). Otherwise, we increment the publish_level
      field to simulate a non confirmed slot (as for even levels, no slot is
      confirmed. See {!populate_slots_history}). *)
  let request_unconfirmed_page (poly, slot, skip_slot) =
    let open Lwt_result_syntax in
    let open Dal_slot_repr.Header in
    if skip_slot then
      let level = slot.id.published_level in
      let* _page_info, page_id = mk_page_info ~level slot poly in
      (* We should not provide the page's info if we want to build an
         unconfirmation proof. *)
      return_some (None, page_id)
    else return_none

  (** This helper function allows to test DAL's {!produce_proof} and
      {!verify_proof} functions, using the data constructed from
      {!populate_slots_history} above. *)
  let helper_check_pbt_pages last_cell last_cache slots_info ~page_to_request
      ~check_produce ~check_verify =
    let open Lwt_result_syntax in
    List.iter_es
      (fun item ->
        let* mk_test = page_to_request item in
        match mk_test with
        | None -> return_unit
        | Some (page_info, page_id) ->
            produce_and_verify_proof
              last_cell
              ~get_history:(get_history last_cache)
              ~page_info
              ~page_id
              ~check_produce
              ~check_verify)
      slots_info

  (** Making some confirmation pages tests for slots that are confirmed. *)
  let test_confirmed_pages (levels_data : levels) =
    let open Lwt_result_syntax in
    let* last_cell, last_cache, slots_info =
      populate_slots_history levels_data
    in
    helper_check_pbt_pages
      last_cell
      last_cache
      slots_info
      ~page_to_request:request_confirmed_page
      ~check_produce:(successful_check_produce_result ~__LOC__ `Confirmed)
      ~check_verify:(successful_check_verify_result ~__LOC__ `Confirmed)

  (** Making some unconfirmation pages tests for slots that are confirmed. *)
  let test_unconfirmed_pages (levels_data : levels) =
    let open Lwt_result_syntax in
    let* last_cell, last_cache, slots_info =
      populate_slots_history levels_data
    in
    helper_check_pbt_pages
      last_cell
      last_cache
      slots_info
      ~page_to_request:request_unconfirmed_page
      ~check_produce:(successful_check_produce_result ~__LOC__ `Unconfirmed)
      ~check_verify:(successful_check_verify_result ~__LOC__ `Unconfirmed)

  let tests =
    let gen_dal_config : levels QCheck2.Gen.t =
      QCheck2.Gen.(
        let nb_slots = 0 -- Parameters.(dal_parameters.number_of_slots) in
        let nb_levels = 4 -- 30 in
        let* start_level =
          let* n = small_nat in
          return (n + 1)
          (* skip level 0 by adding 1 *)
        in
        (* The slot is confirmed iff the boolean is true *)
        let slot = bool in
        let slots = list_size nb_slots slot in
        (* For each level, we generate the gap/delta w.r.t. the previous level,
           and the slots' flags (confirmed or not). *)
        let* list = list_size nb_levels slots in
        List.mapi
          (fun i slots ->
            let level = start_level + i in
            (level, slots))
          list
        |> return)
    in
    [
      Tztest.tztest_qcheck2
        ~name:"Pbt tests: confirmed pages"
        ~count:Parameters.count
        gen_dal_config
        test_confirmed_pages;
      Tztest.tztest_qcheck2
        ~name:"Pbt tests: unconfirmed pages"
        ~count:Parameters.count
        gen_dal_config
        test_unconfirmed_pages;
    ]

  let tests =
    [
      ( Format.sprintf
          "[%s: %s] Dal slots refutation"
          Protocol.name
          Parameters.name,
        tests );
    ]
end

let () =
  let open Tezos_protocol_021_PsQuebec_parameters.Default_parameters in
  let module Test = Make (struct
    let name = "test"

    let count = 5

    let dal_parameters = {constants_test.dal with number_of_slots = 24}
  end) in
  Alcotest_lwt.run
    ~__FILE__
    (Protocol.name ^ ": Dal slots refutation game")
    Test.tests
  |> Lwt_main.run
