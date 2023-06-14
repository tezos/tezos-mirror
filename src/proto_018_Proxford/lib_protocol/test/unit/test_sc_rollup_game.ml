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
    Invocation: dune exec src/proto_018_Proxford/lib_protocol/test/unit/main.exe \
                  -- --file test_sc_rollup_game.ml
    Subject:    Tests for the SCORU refutation game
*)

open Protocol
module Commitment_repr = Sc_rollup_commitment_repr
module T = Test_sc_rollup_storage
module R = Sc_rollup_refutation_storage
module D = Sc_rollup_dissection_chunk_repr
module G = Sc_rollup_game_repr
module Tick = Sc_rollup_tick_repr

(** Assert that the computation fails with the given error. *)
let assert_fails_with ~__LOC__ k expected_err =
  let open Lwt_result_syntax in
  let*! res = k in
  Assert.proto_error ~loc:__LOC__ res (( = ) expected_err)

let assert_fails_with_f ~__LOC__ k f =
  let open Lwt_result_syntax in
  let*! res = k in
  Assert.proto_error ~loc:__LOC__ res f

let tick_of_int_exn n =
  match Tick.of_int n with None -> assert false | Some t -> t

let context_hash_of_string s = Context_hash.hash_string [s]

let hash_string s =
  Sc_rollup_repr.State_hash.context_hash_to_state_hash
  @@ context_hash_of_string s

let hash_int n = hash_string (Format.sprintf "%d" n)

let mk_dissection_chunk (state_hash, tick) = D.{state_hash; tick}

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
  Stdlib.List.init (size + 1) init_tick

let init_refutation ~size ?init_tick start_hash =
  let choice = Sc_rollup_tick_repr.initial in
  let step = G.Dissection (init_dissection ~size ?init_tick start_hash) in
  (choice, step)

let two_stakers_in_conflict () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, rollup, genesis_hash, refuter, defender, staker3 =
    T.originate_rollup_and_deposit_with_three_stakers ()
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
  let level l = T.valid_inbox_level ctxt l in
  let*@ parent, _, ctxt =
    T.advance_level_n_refine_stake ctxt rollup defender parent_commit
  in
  let child1 =
    Commitment_repr.
      {
        predecessor = parent;
        inbox_level = level 2l;
        number_of_ticks = T.number_of_ticks_exn 10000L;
        compressed_state = hash2;
      }
  in
  let child2 =
    Commitment_repr.
      {
        predecessor = parent;
        inbox_level = level 2l;
        number_of_ticks = T.number_of_ticks_exn 10000L;
        compressed_state = hash3;
      }
  in
  let ctxt = T.advance_level_for_commitment ctxt child1 in
  let*@ _, _, ctxt, _ =
    Sc_rollup_stake_storage.publish_commitment ctxt rollup defender child1
  in
  let*@ _, _, ctxt, _ =
    Sc_rollup_stake_storage.publish_commitment ctxt rollup refuter child2
  in
  let defender_commitment_hash =
    Sc_rollup_commitment_repr.hash_uncarbonated child1
  in
  let refuter_commitment_hash =
    Sc_rollup_commitment_repr.hash_uncarbonated child2
  in
  return
    ( ctxt,
      rollup,
      refuter,
      defender,
      staker3,
      refuter_commitment_hash,
      defender_commitment_hash )

(** A dissection is 'poorly distributed' if its tick counts are not
very evenly spread through the total tick-duration. Formally, the
maximum tick-distance between two consecutive states in a dissection
may not be more than half of the total tick-duration. *)
let test_poorly_distributed_dissection () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt,
         rollup,
         refuter,
         defender,
         _staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let start_hash = hash_string "foo" in
  let init_tick size i =
    mk_dissection_chunk
    @@
    if i = size then (None, tick_of_int_exn 10000)
    else (Some (if i = 0 then start_hash else hash_int i), tick_of_int_exn i)
  in
  let player = refuter and opponent = defender in
  let player_commitment_hash = refuter_commitment_hash
  and opponent_commitment_hash = defender_commitment_hash in
  let*@ ctxt =
    R.start_game
      ctxt
      rollup
      ~player:(player, player_commitment_hash)
      ~opponent:(opponent, opponent_commitment_hash)
  in
  let size =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in
  let choice, step = init_refutation ~size ~init_tick start_hash in
  assert_fails_with_f
    ~__LOC__
    (wrap
    @@ R.game_move ctxt rollup ~player:refuter ~opponent:defender ~step ~choice
    )
    (function D.Dissection_invalid_distribution _ -> true | _ -> false)

let test_single_valid_game_move () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt,
         rollup,
         refuter,
         defender,
         _staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let start_hash = hash_string "foo" in
  let size =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in
  let tick_per_state = 10_000 / size in
  let dissection =
    Stdlib.List.init (size + 1) (fun i ->
        mk_dissection_chunk
        @@
        if i = 0 then (Some start_hash, tick_of_int_exn 0)
        else if i = size then (None, tick_of_int_exn 10000)
        else (Some (hash_int i), tick_of_int_exn (i * tick_per_state)))
  in
  let player = refuter and opponent = defender in
  let player_commitment_hash = refuter_commitment_hash
  and opponent_commitment_hash = defender_commitment_hash in

  let*@ ctxt =
    R.start_game
      ctxt
      rollup
      ~player:(player, player_commitment_hash)
      ~opponent:(opponent, opponent_commitment_hash)
  in
  let choice, step = (Sc_rollup_tick_repr.initial, G.Dissection dissection) in
  let*@ game_result, _ctxt =
    R.game_move ctxt rollup ~player:refuter ~opponent:defender ~choice ~step
  in
  Assert.is_none ~loc:__LOC__ ~pp:Sc_rollup_game_repr.pp_game_result game_result

module Arith_pvm = Sc_rollup_helpers.Arith_pvm

(** Test that sending a invalid serialized inbox proof to
    {Sc_rollup_proof_repr.valid} is rejected. *)
let test_invalid_serialized_inbox_proof () =
  let open Lwt_result_wrap_syntax in
  let open Alpha_context in
  let rollup = Sc_rollup.Address.zero in
  let level = Raw_level.(succ root) in
  let inbox = Sc_rollup_helpers.dumb_init level in
  let snapshot = Sc_rollup.Inbox.take_snapshot inbox in
  let dal_snapshot = Dal.Slots_history.genesis in
  let dal_parameters = Default_parameters.constants_mainnet.dal in
  let ctxt = Sc_rollup_helpers.make_empty_context () in
  let empty = Tezos_context_memory.Context_binary.Tree.empty ctxt in
  let*! state = Arith_pvm.initial_state ~empty in
  (* We evaluate the boot sector, so the [input_requested] is a
     [First_after]. *)
  let*! state = Arith_pvm.eval state in
  let*! pvm_step = Arith_pvm.produce_proof ctxt None state in
  let pvm_step = WithExceptions.Result.get_ok ~loc:__LOC__ pvm_step in

  (* We create an obviously invalid inbox *)
  let inbox_proof =
    Sc_rollup.Inbox.Internal_for_tests.serialized_proof_of_string
      "I am the big bad wolf"
  in
  let inbox_proof =
    Sc_rollup.Proof.Inbox_proof
      {level = Raw_level.root; message_counter = Z.zero; proof = inbox_proof}
  in
  let proof = Sc_rollup.Proof.{pvm_step; input_proof = Some inbox_proof} in

  let metadata =
    Sc_rollup.Metadata.{address = rollup; origination_level = level}
  in
  let*! res =
    wrap
    @@ Sc_rollup.Proof.valid
         ~pvm:(module Arith_pvm)
         ~metadata
         snapshot
         Raw_level.root
         dal_snapshot
         dal_parameters.cryptobox_parameters
         ~dal_attestation_lag:dal_parameters.attestation_lag
         proof
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = ) Sc_rollup_proof_repr.Sc_rollup_invalid_serialized_inbox_proof)

let test_first_move_with_swapped_commitment () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt,
         rollup,
         refuter,
         defender,
         _staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let player = refuter
  and opponent = defender
  and player_commitment_hash = refuter_commitment_hash
  and opponent_commitment_hash = defender_commitment_hash in
  let*! res =
    wrap
    @@ R.start_game
         ctxt
         rollup
         ~player:(player, opponent_commitment_hash)
         ~opponent:(opponent, player_commitment_hash)
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = )
       (Sc_rollup_errors.Sc_rollup_wrong_staker_for_conflict_commitment
          (player, opponent_commitment_hash)))

let test_first_move_from_invalid_player () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt,
         rollup,
         _refuter,
         defender,
         staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let opponent = defender
  and player_commitment_hash = refuter_commitment_hash
  and opponent_commitment_hash = defender_commitment_hash in
  let*! res =
    wrap
    @@ R.start_game
         ctxt
         rollup
         ~player:(staker3, player_commitment_hash)
         ~opponent:(opponent, opponent_commitment_hash)
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = )
       (Sc_rollup_errors.Sc_rollup_wrong_staker_for_conflict_commitment
          (staker3, player_commitment_hash)))

let test_first_move_with_invalid_opponent () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt,
         rollup,
         refuter,
         _defender,
         staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let player = refuter
  and player_commitment_hash = refuter_commitment_hash
  and opponent_commitment_hash = defender_commitment_hash in
  let*! res =
    wrap
    @@ R.start_game
         ctxt
         rollup
         ~player:(player, player_commitment_hash)
         ~opponent:(staker3, opponent_commitment_hash)
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = )
       (Sc_rollup_errors.Sc_rollup_wrong_staker_for_conflict_commitment
          (staker3, opponent_commitment_hash)))

let test_first_move_with_invalid_ancestor () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt,
         rollup,
         refuter,
         defender,
         _staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let*@ inbox_level = T.proper_valid_inbox_level (ctxt, rollup) 3 in
  let refuter_commitment =
    let context_hash11 = hash_string "child11" in
    Commitment_repr.
      {
        predecessor = refuter_commitment_hash;
        inbox_level;
        number_of_ticks = T.number_of_ticks_exn 10000L;
        compressed_state = context_hash11;
      }
  in
  let defender_commitment =
    let context_hash21 = hash_string "child21" in
    Commitment_repr.
      {
        predecessor = defender_commitment_hash;
        inbox_level;
        number_of_ticks = T.number_of_ticks_exn 10000L;
        compressed_state = context_hash21;
      }
  in
  let ctxt = T.advance_level_for_commitment ctxt refuter_commitment in
  let* _, _, ctxt, _ =
    wrap
    @@ Sc_rollup_stake_storage.publish_commitment
         ctxt
         rollup
         refuter
         refuter_commitment
  in
  let* _, _, ctxt, _ =
    wrap
    @@ Sc_rollup_stake_storage.publish_commitment
         ctxt
         rollup
         defender
         defender_commitment
  in
  let refuter_commitment_hash =
    Sc_rollup_commitment_repr.hash_uncarbonated refuter_commitment
  in
  let defender_commitment_hash =
    Sc_rollup_commitment_repr.hash_uncarbonated defender_commitment
  in
  let player = refuter
  and opponent = defender
  and player_commitment_hash = refuter_commitment_hash
  and opponent_commitment_hash = defender_commitment_hash in
  let*! res =
    wrap
    @@ R.start_game
         ctxt
         rollup
         ~player:(player, player_commitment_hash)
         ~opponent:(opponent, opponent_commitment_hash)
  in
  Assert.proto_error
    ~loc:__LOC__
    res
    (( = )
       (Sc_rollup_errors.Sc_rollup_not_valid_commitments_conflict
          (player_commitment_hash, player, opponent_commitment_hash, opponent)))

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
      "Invalid serialized inbox proof is rejected."
      `Quick
      test_invalid_serialized_inbox_proof;
    Tztest.tztest
      "start a game with invalid commitment hash (swap commitment)."
      `Quick
      test_first_move_with_swapped_commitment;
    Tztest.tztest
      "start a game with invalid commitment hash (op from outsider)."
      `Quick
      test_first_move_from_invalid_player;
    Tztest.tztest
      "start a game with invalid commitment hash (opponent is not in game)."
      `Quick
      test_first_move_with_invalid_opponent;
    Tztest.tztest
      "start a game with commitment hash that are not the first conflict."
      `Quick
      test_first_move_with_invalid_ancestor;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("sc rollup game", tests)]
  |> Lwt_main.run
