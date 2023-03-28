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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/pbt/main.exe
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
      WithExceptions.Result.get_ok ~loc:__LOC__
      @@ Dal_helpers.mk_cryptobox Parameters.dal_parameters.cryptobox_parameters
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
    let open Result_syntax in
    (* Make and insert a slot. *)
    let slot_data =
      Bytes.init
        Parameters.(dal_parameters.cryptobox_parameters.slot_size)
        (fun _i -> 'x')
    in
    let* polynomial = dal_mk_polynomial_from_slot slot_data in
    let cryptobox = Lazy.force ARG.cryptobox in
    let* commitment = dal_commit cryptobox polynomial in
    let add_slot level sindex (cell, cache, slots_info) skip_slot =
      let index =
        Option.value_f
          (Dal_slot_index_repr.of_int_opt sindex)
          ~default:(fun () -> assert false)
      in
      let slot =
        Dal_slot_repr.Header.{id = {published_level = level; index}; commitment}
      in
      let* cell, cache =
        if skip_slot then return (cell, cache)
        else
          Dal_slot_repr.History.add_confirmed_slot_headers cell cache [slot]
          |> Environment.wrap_tzresult
      in
      return (cell, cache, (polynomial, slot, skip_slot) :: slots_info)
    in
    (* Insert the slots of a level. *)
    let add_slots accu (level, slots_data) =
      (* We start at level one, and we skip even levels for test purpose (which
         means that no DAL slot is confirmed for them). *)
      let curr_level = Raw_level_repr.of_int32_exn (Int32.of_int level) in
      List.fold_left_i_e (add_slot curr_level) accu slots_data
    in
    (* Insert the slots of all the levels. *)
    let add_levels = List.fold_left_e add_slots in
    add_levels (genesis_history, genesis_history_cache, []) levels_data

  (** This function returns the (correct) information of a page to
      prove that it is confirmed, or None if the page's slot is skipped. *)
  let request_confirmed_page (poly, slot, skip_slot) =
    let open Result_syntax in
    if skip_slot then
      (* We cannot check that a page of an unconfirmed slot is confirmed. *)
      return None
    else
      let* page_info, page_id = mk_page_info slot poly in
      return @@ Some (page_info, page_id)

  (** This function returns information of a page to prove that it is
      unconfirmed, if the page's slot is skipped, the information look correct
      (but the slot is not confirmed). Otherwise, we increment the publish_level
      field to simulate a non confirmed slot (as for even levels, no slot is
      confirmed. See {!populate_slots_history}). *)
  let request_unconfirmed_page unconfirmed_level (poly, slot, skip_slot) =
    let open Result_syntax in
    (* If the slot is unconfirmed, we test that a page belonging to it is not
       confirmed.  If the slot is confirmed, we check that the page of the
       slot at the next level is unconfirmed (since we insert levels without
       any confirmed slot). *)
    let level =
      let open Dal_slot_repr.Header in
      if skip_slot then slot.id.published_level else unconfirmed_level
    in
    let* _page_info, page_id = mk_page_info ~level slot poly in
    (* We should not provide the page's info if we want to build an
       unconfirmation proof. *)
    return @@ Some (None, page_id)

  (** This helper function allows to test DAL's {!produce_proof} and
      {!verify_proof} functions, using the data constructed from
      {!populate_slots_history} above. *)
  let helper_check_pbt_pages last_cell last_cache slots_info ~page_to_request
      ~check_produce ~check_verify =
    let open Lwt_result_syntax in
    List.iter_es
      (fun item ->
        let*? mk_test = page_to_request item in
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
    let*? last_cell, last_cache, slots_info =
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
    let*? last_cell, last_cache, slots_info =
      populate_slots_history levels_data
    in
    let unconfirmed_level =
      let last_level =
        List.last (0, []) levels_data
        |> fst |> Int32.of_int |> Raw_level_repr.of_int32_exn
      in
      Raw_level_repr.succ last_level
    in
    helper_check_pbt_pages
      last_cell
      last_cache
      slots_info
      ~page_to_request:(request_unconfirmed_page unconfirmed_level)
      ~check_produce:(successful_check_produce_result ~__LOC__ `Unconfirmed)
      ~check_verify:(successful_check_verify_result ~__LOC__ `Unconfirmed)

  let tests =
    let gen_dal_config : levels QCheck2.Gen.t =
      QCheck2.Gen.(
        let nb_slots = 10 -- 20 in
        let nb_levels = 4 -- 8 in
        let gaps_between_levels = 1 -- 20 in
        (* The slot is confirmed iff the boolean is true *)
        let slot = bool in
        let slots = list_size nb_slots slot in
        (* For each level, we generate the gap/delta w.r.t. the previous level,
           and the slots' flags (confirmed or not). *)
        let* l = list_size nb_levels (pair gaps_between_levels slots) in
        (* We compute the list of slots with explicit levels instead levels
           gaps. *)
        let rl, _level =
          List.fold_left
            (fun (acc, prev_level) (delta_level, slots) ->
              let level = prev_level + delta_level in
              ((level, slots) :: acc, level))
            ([], 0)
            l
        in
        return @@ List.rev rl)
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
  let open Tezos_protocol_alpha_parameters.Default_parameters in
  let module Test = Make (struct
    let name = "test"

    let count = 5

    let dal_parameters = constants_test.dal
  end) in
  Alcotest_lwt.run (Protocol.name ^ ": Dal slots refutation game") Test.tests
  |> Lwt_main.run
