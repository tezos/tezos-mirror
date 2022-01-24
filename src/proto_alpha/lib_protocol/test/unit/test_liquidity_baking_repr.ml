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
      -- test "^\[Unit\] liquidity baking$"
    Subject:    Tests for the Liquidity_baking_repr module
*)

open Protocol
open Block_header_repr

let ema_of_int32 ema =
  Liquidity_baking_repr.Escape_EMA.of_int32 ema >|= Environment.wrap_tzresult

let ema_to_int32 = Liquidity_baking_repr.Escape_EMA.to_int32

let compute_new_ema ~escape_vote ema =
  Liquidity_baking_repr.compute_new_ema ~escape_vote ema |> ema_to_int32

let test_ema_pass () =
  List.iter_es
    (fun old_ema ->
      ema_of_int32 old_ema >>=? fun ema ->
      Assert.equal_int32
        ~loc:__LOC__
        (compute_new_ema ~escape_vote:LB_pass ema)
        old_ema)
    [
      0l;
      1l;
      10l;
      100l;
      10000l;
      10_000l;
      100_000l;
      500_000l;
      1_000_000l;
      1_500_000l;
      1_900_000l;
      1_990_000l;
      1_999_000l;
      1_999_900l;
      1_999_990l;
      1_999_999l;
      2_000_000l;
    ]

(* Test that new_ema is still between 0 and 2,000,000 after an Off vote. *)
let test_ema_in_bound_off () =
  List.iter_es
    (fun old_ema ->
      ema_of_int32 old_ema >>=? fun ema ->
      let new_ema = compute_new_ema ~toggle_vote:LB_off ema in
      Assert.leq_int32 ~loc:__LOC__ 0l new_ema >>=? fun () ->
      Assert.leq_int32 ~loc:__LOC__ new_ema 2_000_000l)
    ema_range

(* Test that new_ema > old_ema when voting Off, except if old_ema is
   already very close to the upper bound. *)
let test_ema_increases_off () =
  List.iter_es
    (fun old_ema ->
      ema_of_int32 old_ema >>=? fun ema ->
      Assert.lt_int32
        ~loc:__LOC__
        old_ema
        (compute_new_ema ~escape_vote:LB_off ema))
    [
      0l;
      1l;
      10l;
      100l;
      10000l;
      10_000l;
      100_000l;
      500_000l;
      1_000_000l;
      1_500_000l;
      1_900_000l;
      1_990_000l;
      1_998_000l;
    ]

(* Test that the increase in EMA caused by an Off vote is bounded by 1,000,000 *)
let test_ema_increases_off_bound () =
  List.iter_es
    (fun old_ema ->
      ema_of_int32 old_ema >>=? fun ema ->
      Assert.leq_int32
        ~loc:__LOC__
        (Int32.sub (compute_new_ema ~escape_vote:LB_off ema) old_ema)
        1000l)
    [
      0l;
      1l;
      10l;
      100l;
      10000l;
      10_000l;
      100_000l;
      500_000l;
      1_000_000l;
      1_500_000l;
      1_900_000l;
      1_990_000l;
      1_999_000l;
      1_999_900l;
      1_999_990l;
      1_999_999l;
      2_000_000l;
    ]

(* Test that new_ema is still between 0 and 2,000,000 after an Off vote. *)
let test_ema_in_bound_on () =
  List.iter_es
    (fun old_ema ->
      ema_of_int32 old_ema >>=? fun ema ->
      let new_ema = compute_new_ema ~toggle_vote:LB_on ema in
      Assert.leq_int32 ~loc:__LOC__ 0l new_ema >>=? fun () ->
      Assert.leq_int32 ~loc:__LOC__ new_ema 2_000_000l)
    ema_range

(* Test that new_ema < old_ema when voting On, except if old_ema is
   already very close to the lower bound. *)
let test_ema_decreases_on () =
  List.iter_es
    (fun old_ema ->
      ema_of_int32 old_ema >>=? fun ema ->
      Assert.lt_int32
        ~loc:__LOC__
        (compute_new_ema ~escape_vote:LB_on ema)
        old_ema)
    [
      1l;
      10l;
      100l;
      10000l;
      10_000l;
      100_000l;
      500_000l;
      1_000_000l;
      1_500_000l;
      1_900_000l;
      1_990_000l;
      1_999_000l;
      1_999_900l;
      1_999_990l;
      1_999_999l;
      2_000_000l;
    ]

(* Test that the decrease in EMA caused by an On vote is bounded by 1,000,000 *)
let test_ema_decreases_on_bound () =
  List.iter_es
    (fun old_ema ->
      ema_of_int32 old_ema >>=? fun ema ->
      Assert.leq_int32
        ~loc:__LOC__
        (Int32.sub (compute_new_ema ~escape_vote:LB_on ema) old_ema)
        1000l)
    [
      0l;
      1l;
      10l;
      100l;
      10000l;
      10_000l;
      100_000l;
      500_000l;
      1_000_000l;
      1_500_000l;
      1_900_000l;
      1_990_000l;
      1_999_000l;
      1_999_900l;
      1_999_990l;
      1_999_999l;
      2_000_000l;
    ]

let tests =
  [
    Tztest.tztest
      "Test EMA does not change when vote is Pass"
      `Quick
      test_ema_pass;
    Tztest.tztest
      "Test EMA remains in bounds when vote is Off"
      `Quick
      test_ema_in_bound_off;
    Tztest.tztest
      "Test EMA increases when vote is Off"
      `Quick
      test_ema_increases_off;
    Tztest.tztest
      "Test EMA does not increase too much when vote is Off"
      `Quick
      test_ema_increases_off_bound;
    Tztest.tztest
      "Test EMA remains in bounds when vote is On"
      `Quick
      test_ema_in_bound_on;
    Tztest.tztest
      "Test EMA decreases when vote is On"
      `Quick
      test_ema_decreases_on;
    Tztest.tztest
      "Test EMA does not decrease too much when vote is On"
      `Quick
      test_ema_decreases_on_bound;
  ]
