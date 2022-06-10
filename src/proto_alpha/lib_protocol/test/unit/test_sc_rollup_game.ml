(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
    Component:  Protocol Sc_rollup_refutation_storage
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
      -- test "^\[Unit\] sc rollup game$"
    Subject:    Tests for the SCORU refutation game
*)

open Protocol
open Lwt_result_syntax
module Commitment_repr = Sc_rollup_commitment_repr
module T = Test_sc_rollup_storage
module R = Sc_rollup_refutation_storage
module Tick = Sc_rollup_tick_repr

let check_reason ~loc (outcome : Sc_rollup_game_repr.outcome option) s =
  match outcome with
  | None -> assert false
  | Some o -> (
      match o.reason with
      | Conflict_resolved -> assert false
      | Timeout -> assert false
      | Invalid_move r ->
          Assert.equal
            ~loc
            String.equal
            "Compare invalid_move reasons"
            Format.pp_print_string
            r
            s)

let tick_of_int_exn n =
  match Tick.of_int n with None -> assert false | Some t -> t

let hash_int n = Sc_rollup_repr.State_hash.hash_string [Format.sprintf "%d" n]

let init_dissection ?(size = 32) start_hash =
  let init_tick i =
    if i = size - 1 then (None, tick_of_int_exn 10000)
    else (Some (if i = 0 then start_hash else hash_int i), tick_of_int_exn i)
  in
  Stdlib.List.init size init_tick

let two_stakers_in_conflict () =
  let* ctxt, rollup, refuter, defender =
    T.originate_rollup_and_deposit_with_two_stakers ()
  in
  let hash1 = Sc_rollup_repr.State_hash.hash_string ["foo"] in
  let hash2 = Sc_rollup_repr.State_hash.hash_string ["bar"] in
  let hash3 = Sc_rollup_repr.State_hash.hash_string ["xyz"] in
  let parent_commit =
    Commitment_repr.
      {
        predecessor = Commitment_repr.Hash.zero;
        inbox_level = T.valid_inbox_level ctxt 1l;
        number_of_messages = T.number_of_messages_exn 5l;
        number_of_ticks = T.number_of_ticks_exn 152231l;
        compressed_state = hash1;
      }
  in
  let* parent, _, ctxt =
    T.lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         defender
         parent_commit
  in
  let child1 =
    Commitment_repr.
      {
        predecessor = parent;
        inbox_level = T.valid_inbox_level ctxt 2l;
        number_of_messages = T.number_of_messages_exn 2l;
        number_of_ticks = T.number_of_ticks_exn 10000l;
        compressed_state = hash2;
      }
  in
  let child2 =
    Commitment_repr.
      {
        predecessor = parent;
        inbox_level = T.valid_inbox_level ctxt 2l;
        number_of_messages = T.number_of_messages_exn 2l;
        number_of_ticks = T.number_of_ticks_exn 10000l;
        compressed_state = hash3;
      }
  in
  let* _, _, ctxt, _ =
    T.lift
    @@ Sc_rollup_stake_storage.publish_commitment ctxt rollup defender child1
  in
  let* _, _, ctxt, _ =
    T.lift
    @@ Sc_rollup_stake_storage.publish_commitment ctxt rollup refuter child2
  in
  return (ctxt, rollup, refuter, defender)

(** A dissection is 'poorly distributed' if its tick counts are not
    very evenly spread through the total tick-duration. Formally, the
    maximum tick-distance between two consecutive states in a dissection
    may not be more than half of the total tick-duration. *)
let test_poorly_distributed_dissection () =
  let* ctxt, rollup, refuter, defender = two_stakers_in_conflict () in
  let start_hash = Sc_rollup_repr.State_hash.hash_string ["foo"] in
  let dissection = init_dissection start_hash in
  let move =
    Sc_rollup_game_repr.
      {choice = Sc_rollup_tick_repr.initial; step = Dissection dissection}
  in
  let* outcome, _ctxt =
    T.lift
    @@ R.game_move
         ctxt
         rollup
         ~player:refuter
         ~opponent:defender
         move
         ~is_opening_move:true
  in
  let expected_reason =
    "Maximum tick increment in dissection must be less than half total \
     dissection length"
  in
  check_reason ~loc:__LOC__ outcome expected_reason

let test_single_valid_game_move () =
  let* ctxt, rollup, refuter, defender = two_stakers_in_conflict () in
  let start_hash = Sc_rollup_repr.State_hash.hash_string ["foo"] in
  let dissection =
    Stdlib.List.init 32 (fun i ->
        if i = 0 then (Some start_hash, tick_of_int_exn 0)
        else if i = 31 then (None, tick_of_int_exn 10000)
        else (Some (hash_int i), tick_of_int_exn (i * 200)))
  in
  let move =
    Sc_rollup_game_repr.
      {choice = Sc_rollup_tick_repr.initial; step = Dissection dissection}
  in
  let* outcome, _ctxt =
    T.lift
    @@ R.game_move
         ctxt
         rollup
         ~player:refuter
         ~opponent:defender
         move
         ~is_opening_move:true
  in
  Assert.is_none ~loc:__LOC__ ~pp:Sc_rollup_game_repr.pp_outcome outcome

let tests =
  [
    Tztest.tztest
      "A badly distributed dissection is an invalid move"
      `Quick
      test_poorly_distributed_dissection;
    Tztest.tztest
      "A single game move with a valid dissection"
      `Quick
      test_single_valid_game_move;
  ]
