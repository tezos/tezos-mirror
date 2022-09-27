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
    Component:  Protocol (dal slot proof)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] dal slot proof$"
    Subject:    These unit tests check proof-related functions of Dal slots.
*)

open Protocol
module S = Dal_slot_repr
module P = S.Page
module Hist = S.Slots_history
open Dal_helpers

(* Tests to check insertion of slots in a dal skip list. *)

(** Check insertion of a new slot in the given skip list. *)
let skip_list_ordering dal skip_list ~mk_level ~mk_slot_index ~check_result =
  let open Lwt_result_syntax in
  let {S.id; _} = Hist.Internal_for_tests.content skip_list in
  let*? _data, _poly, slot =
    mk_slot ~level:(mk_level id) ~index:(mk_slot_index id) dal
  in
  Hist.add_confirmed_slots_no_cache skip_list [slot]
  |> Environment.wrap_tzresult |> check_result

(** This test attempts to add a slot on top of genesis cell zero which would
   break the ordering. In fact, confirmed slots' skip list is ordered by slots
   ID: the slots' level should increase or the level is equal in which case the
   slots' index should increase. In the test below, we attempt to insert a slot
   where (published_level, slot_index) doesn't increase (is the same as the
   genesis cell).  *)
let insertion_breaks_skip_list_ordering dal () =
  skip_list_ordering
    dal
    genesis_history
    ~mk_level:(fun id -> id.S.published_level)
    ~mk_slot_index:(fun id -> id.S.index)
    ~check_result:(fun res ->
      Assert.proto_error ~loc:__LOC__ res (function
          | Hist.Add_element_in_slots_skip_list_violates_ordering -> true
          | _ -> false))

(** This test attempts to add a slot on top of genesis cell zero which satisfies
    the ordering. *)
let correct_insertion_in_skip_list_ordering_1 dal () =
  let open Lwt_result_syntax in
  skip_list_ordering
    dal
    genesis_history
    ~mk_level:(fun id -> Raw_level_repr.succ id.S.published_level)
    ~mk_slot_index:(fun id -> id.S.index)
    ~check_result:(fun res ->
      let* _skip_list = Assert.get_ok ~__LOC__ res in
      return_unit)

(** This test attempts to add a slot on top of genesis cell zero which satisfies
    the ordering. *)
let correct_insertion_in_skip_list_ordering_2 dal () =
  let open Lwt_result_syntax in
  skip_list_ordering
    dal
    genesis_history
    ~mk_level:(fun id -> id.S.published_level)
    ~mk_slot_index:(fun id -> succ_slot_index id.S.index)
    ~check_result:(fun res ->
      let* _skip_list = Assert.get_ok ~__LOC__ res in
      return_unit)

(** This test attempts to add two slots on top of genesis cell zero which satisfies
    the ordering. *)
let correct_insertion_in_skip_list_ordering_3 dal () =
  let open Lwt_result_syntax in
  skip_list_ordering
    dal
    genesis_history
    ~mk_level:(fun id -> id.S.published_level)
    ~mk_slot_index:(fun id -> succ_slot_index id.S.index)
    ~check_result:(fun res ->
      let* skip_list = Assert.get_ok ~__LOC__ res in
      skip_list_ordering
        dal
        skip_list
        ~mk_level:(fun id -> Raw_level_repr.(succ (succ id.S.published_level)))
        ~mk_slot_index:(fun id -> id.S.index)
        ~check_result:(fun res ->
          let* _skip_list = Assert.get_ok ~__LOC__ res in
          return_unit))

(* Tests of construct/verify proofs that confirm/unconfirm pages on top of
   genesis skip list (whose unique cell is slot zero). *)

(** This test attempts to construct a proof to confirm a slot page from the
    genesis skip list. Proof production is expected to fail. *)
let confirmed_page_on_genesis dal () =
  let {S.id = {published_level; index}; _} =
    Hist.Internal_for_tests.content genesis_history
  in
  let page_id = mk_page_id published_level index P.Index.zero in
  produce_and_verify_proof
    dal
    genesis_history
    genesis_history_cache
    (* values of level and slot index are equal to slot zero. We would get a
       page confirmation proof. But, no proof that confirms the existence of a page
       in slot [zero] is possible. *)
    ~page_info:None
    ~page_id
    ~check_produce:(slot_confirmed_but_page_data_not_provided ~__LOC__)

(** This test attempts to construct a proof to unconfirm a slot page from the
    genesis skip list. Proof production is expected to succeed. *)
let unconfirmed_page_on_genesis dal incr_level =
  let {S.id = {published_level; index}; _} =
    Hist.Internal_for_tests.content genesis_history
  in
  let level, sindex =
    if incr_level then (Raw_level_repr.succ published_level, index)
    else (published_level, succ_slot_index index)
  in
  let page_id = mk_page_id level sindex P.Index.zero in
  produce_and_verify_proof
    dal
    genesis_history
    genesis_history_cache
    ~page_info:None
    ~page_id
    ~check_produce:(successful_check_produce_result ~__LOC__ `Unconfirmed)
    ~check_verify:(successful_check_verify_result ~__LOC__ `Unconfirmed)

(* Tests of construct/verify proofs that attempt to confirm pages on top of a
   (confirmed) slot added in genesis_history skip list. *)

(** Helper function that adds a slot a top of the genesis skip list. *)
let helper_confirmed_slot_on_genesis ~level ~mk_page_info ~check_produce
    ?check_verify dal =
  let open Lwt_result_syntax in
  let*? _slot_data, polynomial, slot = mk_slot ~level dal in
  let*? skip_list, cache =
    Hist.add_confirmed_slots genesis_history genesis_history_cache [slot]
    |> Environment.wrap_tzresult
  in
  let*? page_info, page_id = mk_page_info dal slot polynomial in
  produce_and_verify_proof
    dal
    skip_list
    cache
    ~page_info
    ~page_id
    ?check_verify
    ~check_produce

(** Test where a slot is confirmed, requesting a proof for a confirmed page,
   where the correct data and page proof are provided. *)
let confirmed_slot_on_genesis_confirmed_page_good_data dal () =
  helper_confirmed_slot_on_genesis
    dal
    ~level:(Raw_level_repr.succ level_ten)
    ~mk_page_info
    ~check_produce:(successful_check_produce_result ~__LOC__ `Confirmed)
    ~check_verify:(successful_check_verify_result ~__LOC__ `Confirmed)

(** Test where a slot is confirmed, requesting a proof for a confirmed page,
    where the page data and proof are not given. *)
let confirmed_slot_on_genesis_confirmed_page_no_data dal () =
  helper_confirmed_slot_on_genesis
    dal
    ~level:(Raw_level_repr.succ level_ten)
    ~mk_page_info:(mk_page_info ~custom_data:no_data)
    ~check_produce:(slot_confirmed_but_page_data_not_provided ~__LOC__)

(** Test where a slot is confirmed, requesting a proof for a confirmed page,
    where correct data are provided, but the given page proof is wrong. *)
let confirmed_slot_on_genesis_confirmed_page_bad_page_proof dal () =
  let open Result_syntax in
  helper_confirmed_slot_on_genesis
    dal
    ~level:(Raw_level_repr.succ level_ten)
    ~mk_page_info:(fun dal slot poly ->
      let* page_info1, _page_id1 = mk_page_info ~page_index:1 dal slot poly in
      let* page_info2, page_id2 = mk_page_info ~page_index:2 dal slot poly in
      assert (
        match (page_info1, page_info2) with
        | Some (_d1, p1), Some (_d2, p2) -> not (eq_page_proof p1 p2)
        | _ -> false) ;
      return (page_info1, page_id2))
    ~check_produce:
      (failing_check_produce_result
         ~__LOC__
         "Wrong page content for the given page index and slot commitment \
          (page \
          data=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, \
          page id=(published_level: 11, slot_index: 0, page_index: 2), \
          commitment=sh1veuXUPvxu6SWCWtN5v2erwCQVc787gZbFT5LEbixWPLdzb8gemTzAoodnoxJ5HHU2rqu9Ph).")

(** Test where a slot is confirmed, requesting a proof for a confirmed page,
    where correct page proof is provided, but given page data is altered. *)
let confirmed_slot_on_genesis_confirmed_page_bad_data_right_length dal () =
  helper_confirmed_slot_on_genesis
    dal
    ~level:(Raw_level_repr.succ level_ten)
    ~mk_page_info:
      (mk_page_info
         ~custom_data:
           (Some
              (fun ~default_char page_size ->
                Some
                  (Bytes.init page_size (fun i ->
                       if i = 0 then next_char default_char else default_char)))))
    ~check_produce:
      (failing_check_produce_result
         ~__LOC__
         "Wrong page content for the given page index and slot commitment \
          (page \
          data=yxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, \
          page id=(published_level: 11, slot_index: 0, page_index: 0), \
          commitment=sh1veuXUPvxu6SWCWtN5v2erwCQVc787gZbFT5LEbixWPLdzb8gemTzAoodnoxJ5HHU2rqu9Ph).")

(* Variants of the tests above: Construct/verify proofs that attempt to
   unconfirm pages on top of a (confirmed) slot added in genesis_history skip
   list.

   All the tests are somehow equivalent when building "Unconfirmed page" proof,
   because the page's data & page's proof are ignored in this case.
*)

(** Specialisation of helper {!helper_confirmed_slot_on_genesis}, where some
    parameters are fixed. *)
let helper_confirmed_slot_on_genesis_unconfirmed_page ~check_produce
    ?check_verify ~page_level ~mk_page_info dal =
  helper_confirmed_slot_on_genesis
    dal
    ~level:(Raw_level_repr.succ page_level)
    ~mk_page_info
    ~check_produce
    ?check_verify

(** Unconfirmation proof for a page with good data. *)
let confirmed_slot_on_genesis_unconfirmed_page_good_data dal () =
  helper_confirmed_slot_on_genesis_unconfirmed_page
    dal
    ~page_level:level_ten
    ~mk_page_info:(mk_page_info ~level:level_ten)
    ~check_produce:(slot_not_confirmed_but_page_data_provided ~__LOC__)

(** Unconfirmation proof for a page with no data. *)
let confirmed_slot_on_genesis_unconfirmed_page_no_data dal () =
  helper_confirmed_slot_on_genesis_unconfirmed_page
    dal
    ~page_level:level_ten
    ~mk_page_info:(mk_page_info ~custom_data:no_data ~level:level_ten)
    ~check_produce:(successful_check_produce_result ~__LOC__ `Unconfirmed)

(** Unconfirmation proof for a page with bad page proof. *)
let confirmed_slot_on_genesis_unconfirmed_page_bad_proof dal () =
  let open Result_syntax in
  let level = level_ten in
  helper_confirmed_slot_on_genesis_unconfirmed_page
    dal
    ~page_level:level
    ~mk_page_info:(fun dal slot poly ->
      let* page_info1, _page_id1 =
        mk_page_info ~level:level_ten ~page_index:1 dal slot poly
      in
      let* _page_info2, page_id2 =
        mk_page_info ~level:level_ten ~page_index:2 dal slot poly
      in
      assert (
        match (page_info1, _page_info2) with
        | Some (_d1, p1), Some (_d2, p2) -> not (eq_page_proof p1 p2)
        | _ -> false) ;
      return (page_info1, page_id2))
    ~check_produce:(slot_not_confirmed_but_page_data_provided ~__LOC__)

(** Unconfirmation proof for a page with bad data. *)
let confirmed_slot_on_genesis_unconfirmed_page_bad_data dal () =
  let level = level_ten in
  helper_confirmed_slot_on_genesis_unconfirmed_page
    dal
    ~page_level:level
    ~mk_page_info:
      (mk_page_info
         ~level:level_ten
         ~custom_data:
           (Some
              (fun ~default_char page_size ->
                Some
                  (Bytes.init page_size (fun i ->
                       if i = 0 then next_char default_char else default_char)))))
    ~check_produce:(slot_not_confirmed_but_page_data_provided ~__LOC__)

(* The list of tests. *)
let tests =
  Result.value_f
    (dal_mk_env
       {
         Hist.redundancy_factor = 16;
         page_size = 4096 / 64;
         slot_size = 1048576 / 64;
         number_of_shards = 2048 / 64;
       })
    ~default:(fun () ->
      Format.eprintf "failed to initialize Cryptobox.t" ;
      assert false)
  |> fun dal ->
  let tztest title test_function =
    Tztest.tztest title `Quick (test_function dal)
  in
  let qcheck2 name gen test =
    Tztest.tztest_qcheck2 ~name ~count:1 gen (test dal)
  in
  let bool = QCheck2.Gen.bool in
  let ordering_tests =
    [
      tztest
        "add a slot on top of genesis that breaks ordering"
        insertion_breaks_skip_list_ordering;
      tztest
        "add a slot on top of genesis that satisfies ordering (1/2)"
        correct_insertion_in_skip_list_ordering_1;
      tztest
        "add a slot on top of genesis that satisfies ordering (2/2)"
        correct_insertion_in_skip_list_ordering_2;
      tztest
        "add two slots on top of genesis that satisfy ordering"
        correct_insertion_in_skip_list_ordering_3;
    ]
  in
  let proofs_tests_on_genesis =
    [
      tztest "Confirmed page on genesis" confirmed_page_on_genesis;
      qcheck2 "Unconfirmed page on genesis" bool unconfirmed_page_on_genesis;
    ]
  in

  let confirmed_slot_on_genesis_confirmed_page_tests =
    [
      tztest
        "Confirmed slot on top of genesis: confirmed page with good data"
        confirmed_slot_on_genesis_confirmed_page_good_data;
      tztest
        "Confirmed slot on top of genesis: confirmed page with no data"
        confirmed_slot_on_genesis_confirmed_page_no_data;
      tztest
        "Confirmed slot on top of genesis: confirmed page with bad proof"
        confirmed_slot_on_genesis_confirmed_page_bad_page_proof;
      tztest
        "Confirmed slot on top of genesis: confirmed page with bad data "
        confirmed_slot_on_genesis_confirmed_page_bad_data_right_length;
    ]
  in
  let confirmed_slot_on_genesis_unconfirmed_page_tests =
    [
      tztest
        "Confirmed slot on top of genesis: unconfirmed page with good data"
        confirmed_slot_on_genesis_unconfirmed_page_good_data;
      tztest
        "Confirmed slot on top of genesis: unconfirmed page with no data"
        confirmed_slot_on_genesis_unconfirmed_page_no_data;
      tztest
        "Confirmed slot on top of genesis: unconfirmed page with bad proof"
        confirmed_slot_on_genesis_unconfirmed_page_bad_proof;
      tztest
        "Confirmed slot on top of genesis: unconfirmed page with bad data \
         (altered)"
        confirmed_slot_on_genesis_unconfirmed_page_bad_data;
    ]
  in
  ordering_tests @ proofs_tests_on_genesis
  @ confirmed_slot_on_genesis_confirmed_page_tests
  @ confirmed_slot_on_genesis_unconfirmed_page_tests
