(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol Liquidity_baking_repr module
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_liquidity_baking_repr.ml
    Subject:    Tests for the Liquidity_baking_repr module
*)

open Protocol

let ema_of_int64 ema = Toggle_EMA.of_int64 ema >|= Environment.wrap_tzresult

let ema_to_int64 = Toggle_EMA.to_int64

let compute_new_ema ~toggle_vote ema =
  Toggle_votes_repr.compute_new_ema ~toggle_vote ema |> ema_to_int64

(* Folds compute_new_ema on a list of votes *)
let compute_new_ema_n toggle_votes initial_ema =
  List.fold_left
    (fun ema toggle_vote -> Toggle_votes_repr.compute_new_ema ~toggle_vote ema)
    initial_ema
    toggle_votes
  |> ema_to_int64

let ema_range =
  [
    0L;
    1L;
    10L;
    100L;
    1000L;
    10_000L;
    100_000L;
    1_000_000L;
    10_000_000L;
    100_000_000L;
    200_000_000L;
    300_000_000L;
    400_000_000L;
    500_000_000L;
    600_000_000L;
    760_000_000L;
    800_000_000L;
    900_000_000L;
    1_000_000_000L;
    1_100_000_000L;
    1_200_000_000L;
    1_300_000_000L;
    1_400_000_000L;
    1_500_000_000L;
    1_600_000_000L;
    1_700_000_000L;
    1_800_000_000L;
    1_900_000_000L;
    1_990_000_000L;
    1_999_000_000L;
    1_999_900_000L;
    1_999_990_000L;
    1_999_999_000L;
    1_999_999_900L;
    1_999_999_990L;
    1_999_999_999L;
    2_000_000_000L;
  ]

(* Test that new_ema = old_ema when voting Pass. *)
let test_ema_pass () =
  List.iter_es
    (fun old_ema ->
      ema_of_int64 old_ema >>=? fun ema ->
      Assert.equal_int64
        ~loc:__LOC__
        (compute_new_ema ~toggle_vote:Toggle_vote_pass ema)
        old_ema)
    ema_range

(* Test that new_ema is still between 0 and 2,000,000,000 after an Off vote. *)
let test_ema_in_bound_off () =
  List.iter_es
    (fun old_ema ->
      ema_of_int64 old_ema >>=? fun ema ->
      let new_ema = compute_new_ema ~toggle_vote:Toggle_vote_off ema in
      Assert.leq_int64 ~loc:__LOC__ 0L new_ema >>=? fun () ->
      Assert.leq_int64 ~loc:__LOC__ new_ema 2_000_000_000L)
    ema_range

(* Test that new_ema > old_ema when voting Off, except if old_ema is
   already very close to the upper bound. *)
let test_ema_increases_off () =
  List.iter_es
    (fun old_ema ->
      ema_of_int64 old_ema >>=? fun ema ->
      Assert.lt_int64
        ~loc:__LOC__
        old_ema
        (compute_new_ema ~toggle_vote:Toggle_vote_off ema))
    (List.filter (fun ema -> Compare.Int64.(ema < 1_999_999_000L)) ema_range)

(* Test that the increase in EMA caused by an Off vote is bounded by 1,000,000 *)
let test_ema_increases_off_bound () =
  List.iter_es
    (fun old_ema ->
      ema_of_int64 old_ema >>=? fun ema ->
      Assert.leq_int64
        ~loc:__LOC__
        (Int64.sub (compute_new_ema ~toggle_vote:Toggle_vote_off ema) old_ema)
        1_000_000L)
    ema_range

(* Test that new_ema is still between 0 and 2,000,000,000 after an Off vote. *)
let test_ema_in_bound_on () =
  List.iter_es
    (fun old_ema ->
      ema_of_int64 old_ema >>=? fun ema ->
      let new_ema = compute_new_ema ~toggle_vote:Toggle_vote_on ema in
      Assert.leq_int64 ~loc:__LOC__ 0L new_ema >>=? fun () ->
      Assert.leq_int64 ~loc:__LOC__ new_ema 2_000_000_000L)
    ema_range

(* Test that new_ema < old_ema when voting On, except if old_ema is
   already very close to the lower bound. *)
let test_ema_decreases_on () =
  List.iter_es
    (fun old_ema ->
      ema_of_int64 old_ema >>=? fun ema ->
      Assert.lt_int64
        ~loc:__LOC__
        (compute_new_ema ~toggle_vote:Toggle_vote_on ema)
        old_ema)
    (List.filter (fun ema -> Compare.Int64.(ema > 1000L)) ema_range)

(* Test that the decrease in EMA caused by an On vote is bounded by 1,000,000 *)
let test_ema_decreases_on_bound () =
  List.iter_es
    (fun old_ema ->
      ema_of_int64 old_ema >>=? fun ema ->
      Assert.leq_int64
        ~loc:__LOC__
        (Int64.sub (compute_new_ema ~toggle_vote:Toggle_vote_on ema) old_ema)
        1_000_000L)
    ema_range

(* Test that 1385 Off votes are needed to reach the threshold from 0. *)
let test_ema_many_off () =
  let open Toggle_votes_repr in
  ema_of_int64 0L >>=? fun initial_ema ->
  Assert.leq_int64
    ~loc:__LOC__
    (compute_new_ema_n
       (Stdlib.List.init 1385 (fun _ -> Toggle_vote_off))
       initial_ema)
    1_000_000_000L
  >>=? fun () ->
  Assert.leq_int64
    ~loc:__LOC__
    1_000_000_000L
    (compute_new_ema_n
       (Stdlib.List.init 1386 (fun _ -> Toggle_vote_off))
       initial_ema)

(* Test that 1385 On votes are needed to reach the threshold from the max value of the EMA (2,000,000,000). *)
let test_ema_many_on () =
  let open Toggle_votes_repr in
  ema_of_int64 2_000_000_000L >>=? fun initial_ema ->
  Assert.leq_int64
    ~loc:__LOC__
    1_000_000_000L
    (compute_new_ema_n
       (Stdlib.List.init 1385 (fun _ -> Toggle_vote_on))
       initial_ema)
  >>=? fun () ->
  Assert.leq_int64
    ~loc:__LOC__
    (compute_new_ema_n
       (Stdlib.List.init 1386 (fun _ -> Toggle_vote_on))
       initial_ema)
    1_000_000_000L

(* Test that the EMA update function is symmetric:
   from two dual values of the EMA (that is, two values x and y such that
   x + y = 2,000,000,000), voting On on the first one decreases it by as
   much than voting Off on the second one increases it.
*)
let test_ema_symmetry () =
  List.iter_es
    (fun ema ->
      let opposite_ema = Int64.(sub 2_000_000_000L ema) in
      ema_of_int64 ema >>=? fun ema ->
      ema_of_int64 opposite_ema >>=? fun opposite_ema ->
      let new_ema = compute_new_ema ~toggle_vote:Toggle_vote_on ema in
      let new_opposite_ema =
        compute_new_ema ~toggle_vote:Toggle_vote_off opposite_ema
      in
      Assert.equal_int64
        ~loc:__LOC__
        Int64.(add new_ema new_opposite_ema)
        2_000_000_000L)
    ema_range

let tests =
  [
    Tztest.tztest "EMA does not change when vote is Pass" `Quick test_ema_pass;
    Tztest.tztest
      "EMA remains in bounds when vote is Off"
      `Quick
      test_ema_in_bound_off;
    Tztest.tztest "EMA increases when vote is Off" `Quick test_ema_increases_off;
    Tztest.tztest
      "EMA does not increase too much when vote is Off"
      `Quick
      test_ema_increases_off_bound;
    Tztest.tztest
      "EMA remains in bounds when vote is On"
      `Quick
      test_ema_in_bound_on;
    Tztest.tztest "EMA decreases when vote is On" `Quick test_ema_decreases_on;
    Tztest.tztest
      "EMA does not decrease too much when vote is On"
      `Quick
      test_ema_decreases_on_bound;
    Tztest.tztest
      "EMA goes from 0 to one billion in 1386 Off votes"
      `Quick
      test_ema_many_off;
    Tztest.tztest
      "EMA goes from two billions to one billion in 1386 On votes"
      `Quick
      test_ema_many_on;
    Tztest.tztest
      "voting On and Off have symmetric effects on the EMA"
      `Quick
      test_ema_symmetry;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("liquidity baking", tests)]
  |> Lwt_main.run
