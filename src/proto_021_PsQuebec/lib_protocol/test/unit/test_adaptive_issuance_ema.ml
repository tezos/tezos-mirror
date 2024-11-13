(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol, Adaptive Issuance
    Invocation: dune exec src/proto_021_PtQenaB1/lib_protocol/test/unit/main.exe \
                  -- --file test_adaptive_issuance_ema.ml
    Subject:    Tests for the update functions of the EMA of the launch vote of
                Adaptive Issuance
*)

open Protocol
module Votes_EMA = Per_block_votes_repr.Adaptive_issuance_launch_EMA

let ema_of_int32 ema =
  let open Lwt_result_wrap_syntax in
  let*@ result = Votes_EMA.of_int32 ema in
  return result

let ema_to_int32 = Votes_EMA.to_int32

let compute_new_ema ~per_block_vote ema =
  Per_block_votes_repr.compute_new_adaptive_issuance_ema ~per_block_vote ema
  |> ema_to_int32

(* Folds compute_new_ema on a list of votes *)
let compute_new_ema_n per_block_votes initial_ema =
  List.fold_left
    (fun ema per_block_vote ->
      Per_block_votes_repr.compute_new_adaptive_issuance_ema ~per_block_vote ema)
    initial_ema
    per_block_votes
  |> ema_to_int32

let ema_range =
  [
    0l;
    1l;
    10l;
    100l;
    1000l;
    10_000l;
    100_000l;
    1_000_000l;
    10_000_000l;
    100_000_000l;
    200_000_000l;
    300_000_000l;
    400_000_000l;
    500_000_000l;
    600_000_000l;
    760_000_000l;
    800_000_000l;
    900_000_000l;
    1_000_000_000l;
    1_100_000_000l;
    1_200_000_000l;
    1_300_000_000l;
    1_400_000_000l;
    1_500_000_000l;
    1_600_000_000l;
    1_700_000_000l;
    1_800_000_000l;
    1_900_000_000l;
    1_990_000_000l;
    1_999_000_000l;
    1_999_900_000l;
    1_999_990_000l;
    1_999_999_000l;
    1_999_999_900l;
    1_999_999_990l;
    1_999_999_999l;
    2_000_000_000l;
  ]

(* Test that new_ema = old_ema when voting Pass. *)
let test_ema_pass () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun old_ema ->
      let* ema = ema_of_int32 old_ema in
      Assert.equal_int32
        ~loc:__LOC__
        (compute_new_ema ~per_block_vote:Per_block_vote_pass ema)
        old_ema)
    ema_range

(* Test that new_ema is still between 0 and 2,000,000,000 after an On vote. *)
let test_ema_in_bound_on () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun old_ema ->
      let* ema = ema_of_int32 old_ema in
      let new_ema = compute_new_ema ~per_block_vote:Per_block_vote_on ema in
      let* () = Assert.leq_int32 ~loc:__LOC__ 0l new_ema in
      Assert.leq_int32 ~loc:__LOC__ new_ema 2_000_000_000l)
    ema_range

(* Test that new_ema > old_ema when voting On, except if old_ema is
   already very close to the upper bound. *)
let test_ema_increases_on () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun old_ema ->
      let* ema = ema_of_int32 old_ema in
      Assert.lt_int32
        ~loc:__LOC__
        old_ema
        (compute_new_ema ~per_block_vote:Per_block_vote_on ema))
    (List.filter (fun ema -> Compare.Int32.(ema < 1_999_000_000l)) ema_range)

(* Test that the increase in EMA caused by an On vote is bounded by 1,000,000 *)
let test_ema_increases_on_bound () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun old_ema ->
      let* ema = ema_of_int32 old_ema in
      Assert.leq_int32
        ~loc:__LOC__
        (Int32.sub
           (compute_new_ema ~per_block_vote:Per_block_vote_on ema)
           old_ema)
        1_000_000l)
    ema_range

(* Test that new_ema is still between 0 and 2,000,000,000 after an Off vote. *)
let test_ema_in_bound_off () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun old_ema ->
      let* ema = ema_of_int32 old_ema in
      let new_ema = compute_new_ema ~per_block_vote:Per_block_vote_off ema in
      let* () = Assert.leq_int32 ~loc:__LOC__ 0l new_ema in
      Assert.leq_int32 ~loc:__LOC__ new_ema 2_000_000_000l)
    ema_range

(* Test that new_ema < old_ema when voting Off, except if old_ema is
   already very close to the lower bound. *)
let test_ema_decreases_off () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun old_ema ->
      let* ema = ema_of_int32 old_ema in
      Assert.lt_int32
        ~loc:__LOC__
        (compute_new_ema ~per_block_vote:Per_block_vote_off ema)
        old_ema)
    (List.filter (fun ema -> Compare.Int32.(ema > 1_000_000l)) ema_range)

(* Test that the decrease in EMA caused by an Off vote is bounded by 1,000,000 *)
let test_ema_decreases_off_bound () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun old_ema ->
      let* ema = ema_of_int32 old_ema in
      Assert.leq_int32
        ~loc:__LOC__
        (Int32.sub
           (compute_new_ema ~per_block_vote:Per_block_vote_off ema)
           old_ema)
        1_000_000l)
    ema_range

(* Test that 151201 On votes are needed to move from 0% to 50%. *)
let test_ema_many_on () =
  let open Lwt_result_syntax in
  let open Per_block_votes_repr in
  let* initial_ema = ema_of_int32 0l in
  let* () =
    Assert.leq_int32
      ~loc:__LOC__
      (compute_new_ema_n
         (Stdlib.List.init 80321 (fun _ -> Per_block_vote_on))
         initial_ema)
      1_000_000_000l
  in
  Assert.leq_int32
    ~loc:__LOC__
    1_000_000_000l
    (compute_new_ema_n
       (Stdlib.List.init 151201 (fun _ -> Per_block_vote_on))
       initial_ema)

(* Test that 151201 Off votes are needed to move from 100% to 50%. *)
let test_ema_many_off () =
  let open Lwt_result_syntax in
  let open Per_block_votes_repr in
  let* initial_ema = ema_of_int32 2_000_000_000l in
  let* () =
    Assert.leq_int32
      ~loc:__LOC__
      1_000_000_000l
      (compute_new_ema_n
         (Stdlib.List.init 80321 (fun _ -> Per_block_vote_off))
         initial_ema)
  in
  Assert.leq_int32
    ~loc:__LOC__
    (compute_new_ema_n
       (Stdlib.List.init 151201 (fun _ -> Per_block_vote_off))
       initial_ema)
    1_000_000_000l

(* Test that 187259 On votes are needed to move from 0% to 80%. *)
let test_ema_many_many_on () =
  let open Lwt_result_syntax in
  let open Per_block_votes_repr in
  let* initial_ema = ema_of_int32 0l in
  let* () =
    Assert.leq_int32
      ~loc:__LOC__
      (compute_new_ema_n
         (Stdlib.List.init 351122 (fun _ -> Per_block_vote_on))
         initial_ema)
      1_600_000_000l
  in
  Assert.leq_int32
    ~loc:__LOC__
    1_600_000_000l
    (compute_new_ema_n
       (Stdlib.List.init 351123 (fun _ -> Per_block_vote_on))
       initial_ema)

(* Test that the EMA update function is symmetric:
   from two dual values of the EMA (that is, two values x and y such that
   x + y = 2,000,000,000), voting Off on the first one decreases it by as
   much than voting On on the second one increases it.
*)
let test_ema_symmetry () =
  let open Lwt_result_syntax in
  List.iter_es
    (fun ema ->
      let opposite_ema = Int32.(sub 2_000_000_000l ema) in
      let* ema = ema_of_int32 ema in
      let* opposite_ema = ema_of_int32 opposite_ema in
      let new_ema = compute_new_ema ~per_block_vote:Per_block_vote_off ema in
      let new_opposite_ema =
        compute_new_ema ~per_block_vote:Per_block_vote_on opposite_ema
      in
      Assert.equal_int32
        ~loc:__LOC__
        Int32.(add new_ema new_opposite_ema)
        2_000_000_000l)
    ema_range

let tests =
  [
    Tztest.tztest "EMA does not change when vote is Pass" `Quick test_ema_pass;
    Tztest.tztest
      "EMA remains in bounds when vote is On"
      `Quick
      test_ema_in_bound_on;
    Tztest.tztest "EMA increases when vote is On" `Quick test_ema_increases_on;
    Tztest.tztest
      "EMA does not increase too much when vote is On"
      `Quick
      test_ema_increases_on_bound;
    Tztest.tztest
      "EMA remains in bounds when vote is Off"
      `Quick
      test_ema_in_bound_off;
    Tztest.tztest "EMA decreases when vote is Off" `Quick test_ema_decreases_off;
    Tztest.tztest
      "EMA does not decrease too much when vote is Off"
      `Quick
      test_ema_decreases_off_bound;
    Tztest.tztest
      "EMA goes from 0% to 50% in 151201 On votes"
      `Quick
      test_ema_many_on;
    Tztest.tztest
      "EMA goes from 100% to 50% in 151201 Off votes"
      `Quick
      test_ema_many_off;
    Tztest.tztest
      "EMA goes from 0% to 80% in 351117 On votes"
      `Quick
      test_ema_many_many_on;
    Tztest.tztest
      "voting On and Off have symmetric effects on the EMA"
      `Quick
      test_ema_symmetry;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("adaptive issuance ema", tests)]
  |> Lwt_main.run
