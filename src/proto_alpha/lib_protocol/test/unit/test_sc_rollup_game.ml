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
module G = Sc_rollup_game_repr
module Tick = Sc_rollup_tick_repr

(** Assert that the computation fails with the given error. *)
let assert_fails_with ~__LOC__ k expected_err =
  let*! res = k in
  Assert.proto_error ~loc:__LOC__ res (( = ) expected_err)

let tick_of_int_exn n =
  match Tick.of_int n with None -> assert false | Some t -> t

let context_hash_of_string s = Context_hash.hash_string [s]

let hash_string s =
  Sc_rollup_repr.State_hash.context_hash_to_state_hash
  @@ context_hash_of_string s

let hash_int n = hash_string (Format.sprintf "%d" n)

let mk_dissection_chunk (state_hash, tick) = G.{state_hash; tick}

let init_dissection ~size ?init_tick start_hash =
  let default_init_tick i =
    let hash =
      if i = size - 1 then None
      else Some (if i = 0 then start_hash else hash_int i)
    in
    mk_dissection_chunk (hash, tick_of_int_exn i)
  in
  let init_tick =
    Option.fold
      ~none:default_init_tick
      ~some:(fun init_tick -> init_tick size)
      init_tick
  in
  Stdlib.List.init size init_tick

let init_refutation ~size ?init_tick start_hash =
  G.
    {
      choice = Sc_rollup_tick_repr.initial;
      step = Dissection (init_dissection ~size ?init_tick start_hash);
    }

let two_stakers_in_conflict () =
  let* ctxt, rollup, genesis_hash, refuter, defender =
    T.originate_rollup_and_deposit_with_two_stakers ()
  in
  let hash1 = hash_string "foo" in
  let hash2 = hash_string "bar" in
  let hash3 = hash_string "xyz" in
  let parent_commit =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = T.valid_inbox_level ctxt 1l;
        number_of_ticks = T.number_of_ticks_exn 152231L;
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
        number_of_ticks = T.number_of_ticks_exn 10000L;
        compressed_state = hash2;
      }
  in
  let child2 =
    Commitment_repr.
      {
        predecessor = parent;
        inbox_level = T.valid_inbox_level ctxt 2l;
        number_of_ticks = T.number_of_ticks_exn 10000L;
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
  let start_hash = hash_string "foo" in
  let init_tick size i =
    mk_dissection_chunk
    @@
    if i = size - 1 then (None, tick_of_int_exn 10000)
    else (Some (if i = 0 then start_hash else hash_int i), tick_of_int_exn i)
  in
  let* ctxt =
    T.lift @@ R.start_game ctxt rollup ~player:refuter ~opponent:defender
  in
  let size =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in
  let move = init_refutation ~size ~init_tick start_hash in
  assert_fails_with
    ~__LOC__
    (T.lift @@ R.game_move ctxt rollup ~player:refuter ~opponent:defender move)
    Sc_rollup_game_repr.Dissection_invalid_distribution

let test_single_valid_game_move () =
  let* ctxt, rollup, refuter, defender = two_stakers_in_conflict () in
  let start_hash = hash_string "foo" in
  let size =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in
  let tick_per_state = 10_000 / size in
  let dissection =
    Stdlib.List.init size (fun i ->
        mk_dissection_chunk
        @@
        if i = 0 then (Some start_hash, tick_of_int_exn 0)
        else if i = size - 1 then (None, tick_of_int_exn 10000)
        else (Some (hash_int i), tick_of_int_exn (i * tick_per_state)))
  in
  let* ctxt =
    T.lift @@ R.start_game ctxt rollup ~player:refuter ~opponent:defender
  in
  let move =
    Sc_rollup_game_repr.
      {choice = Sc_rollup_tick_repr.initial; step = Dissection dissection}
  in
  let* game_result, _ctxt =
    T.lift @@ R.game_move ctxt rollup ~player:refuter ~opponent:defender move
  in
  Assert.is_none ~loc:__LOC__ ~pp:Sc_rollup_game_repr.pp_game_result game_result

(** Test that a staker can be part of at most one refutation game. *)
let test_staker_injectivity () =
  let open Lwt_result_syntax in
  (* Create the defender and the two refuters. *)
  let* ctxt, rollup, genesis_hash, refuter1, refuter2, operator =
    T.originate_rollup_and_deposit_with_three_stakers ()
  in
  (* Create and publish four commits:
       - [commit1]: the base commit published by [operator] and that everybody
         agrees on;
     and then three commits whose [commit1] is the predecessor and that will
     be challenged in the refutation game:
       - [commit2]: published by [operator];
       - [commit3]: published by [refuter1];
       - [commit4]: published by [refuter2].
  *)
  let hash1 = hash_string "foo" in
  let hash2 = hash_string "bar" in
  let hash3 = hash_string "xyz" in
  let hash4 = hash_string "abc" in
  let agreed_commit =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = T.valid_inbox_level ctxt 1l;
        number_of_ticks = T.number_of_ticks_exn 152231L;
        compressed_state = hash1;
      }
  in
  let* c1_hash, _, ctxt =
    T.lift
    @@ Sc_rollup_stake_storage.Internal_for_tests.refine_stake
         ctxt
         rollup
         operator
         agreed_commit
  in
  let challenging_commit compressed_state =
    Commitment_repr.
      {
        predecessor = c1_hash;
        inbox_level = T.valid_inbox_level ctxt 2l;
        number_of_ticks = T.number_of_ticks_exn 10000L;
        compressed_state;
      }
  in
  let* ctxt =
    List.fold_left_es
      (fun ctxt (hash, player) ->
        let commit = challenging_commit hash in
        let* _, _, ctxt, _ =
          T.lift
          @@ Sc_rollup_stake_storage.publish_commitment
               ctxt
               rollup
               player
               commit
        in
        return ctxt)
      ctxt
      [(hash2, operator); (hash3, refuter1); (hash4, refuter2)]
  in
  (* We start a game [operator <-> refuter1], it succeeds, neither of them
     were in a game before. *)
  let* ctxt =
    T.lift @@ R.start_game ctxt rollup ~player:operator ~opponent:refuter1
  in
  (* We start a game [operator <-> refuter2], it must fail as [operator] is
     already playing against [refuter1]. *)
  let*! res =
    T.lift @@ R.start_game ctxt rollup ~player:operator ~opponent:refuter2
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = ) (Sc_rollup_errors.Sc_rollup_staker_in_game (`Refuter operator)))

module Arith_pvm = Sc_rollup_helpers.Arith_pvm

(** Test that sending a invalid serialized inbox proof to
    {Sc_rollup_proof_repr.valid} is rejected. *)
let test_invalid_serialized_inbox_proof () =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* ctxt = Test_sc_rollup_inbox.create_context () in
  let rollup = Sc_rollup.Address.zero in
  let level = Raw_level.(succ root) in
  let*! inbox = Sc_rollup.Inbox.empty ctxt level in
  let snapshot = Sc_rollup.Inbox.take_snapshot inbox in

  let ctxt = Tezos_context_memory.make_empty_context () in
  let*! state = Arith_pvm.initial_state ctxt in
  (* We evaluate the boot sector, so the [input_requested] is a
     [First_after]. *)
  let*! state = Arith_pvm.eval state in
  let*! proof = Arith_pvm.produce_proof ctxt None state in
  let proof = WithExceptions.Result.get_ok ~loc:__LOC__ proof in
  let wrapped_proof =
    Sc_rollup.Arith_pvm_with_proof
      (module struct
        include Arith_pvm

        let proof = proof
      end)
  in

  (* We create an obviously invalid inbox *)
  let inbox_proof =
    Sc_rollup.Inbox.Internal_for_tests.serialized_proof_of_string
      "I am the big bad wolf"
  in
  let inbox_proof =
    Sc_rollup.Proof.Inbox_proof
      {level = Raw_level.root; message_counter = Z.zero; proof = inbox_proof}
  in
  let proof =
    Sc_rollup.Proof.{pvm_step = wrapped_proof; input_proof = Some inbox_proof}
  in

  let metadata =
    Sc_rollup.Metadata.{address = rollup; origination_level = level}
  in
  let*! res =
    T.lift
    @@ Sc_rollup.Proof.valid
         ~metadata
         snapshot
         Raw_level.root
         ~pvm_name:"arith"
         proof
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = ) Sc_rollup_proof_repr.Sc_rollup_invalid_serialized_inbox_proof)

let tests =
  [
    Tztest.tztest
      "A badly distributed dissection is an invalid move."
      `Quick
      test_poorly_distributed_dissection;
    Tztest.tztest
      "A single game move with a valid dissection"
      `Quick
      test_single_valid_game_move;
    Tztest.tztest
      "Staker can be in at most one game (injectivity)."
      `Quick
      test_staker_injectivity;
    Tztest.tztest
      "Invalid serialized inbox proof is rejected."
      `Quick
      test_invalid_serialized_inbox_proof;
  ]
