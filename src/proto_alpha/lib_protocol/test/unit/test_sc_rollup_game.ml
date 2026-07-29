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
  let*!@ res =
    R.game_move ctxt rollup ~player:refuter ~opponent:defender ~step ~choice
  in
  assert_fails_with_f ~__LOC__ (Lwt.return res) (function
    | D.Dissection_invalid_distribution _ -> true
    | _ -> false)

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
module Arith_pvm_eval = Sc_rollup_helpers.Arith_pvm_eval

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
  let constants = Default_parameters.constants_mainnet in
  let dal_activation_level =
    if constants.dal.feature_enable then
      Some constants.sc_rollup.reveal_activation_level.dal_parameters
    else None
  in
  let dal_attested_slots_validity_lag =
    constants.sc_rollup.reveal_activation_level.dal_attested_slots_validity_lag
  in
  let ctxt = Sc_rollup_helpers.Arith_pvm.make_empty_context () in
  let empty = Sc_rollup_helpers.Arith_pvm.make_empty_state () in
  let*! state = Arith_pvm.initial_state ~empty in
  (* We evaluate the boot sector, so the [input_requested] is a
     [First_after]. *)
  let*! state = Arith_pvm.eval state in
  let is_reveal_enabled = Sc_rollup_helpers.is_reveal_enabled_default in
  let*! pvm_step = Arith_pvm.produce_proof ctxt ~is_reveal_enabled None state in
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
  let*!@ res =
    Sc_rollup.Proof.valid
      ~pvm:(module Arith_pvm)
      ~metadata
      snapshot
      Raw_level.root
      dal_snapshot
      ~dal_activation_level
      ~dal_attested_slots_validity_lag
      ~find_dal_parameters:(fun _ ->
        (* The proof doesn't include a DAL step so finding the parameters is in
           practice not used. *)
        assert false)
      ~is_reveal_enabled
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
  let*!@ res =
    R.start_game
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
  let*!@ res =
    R.start_game
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
  let*!@ res =
    R.start_game
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
  let*@ _, _, ctxt, _ =
    Sc_rollup_stake_storage.publish_commitment
      ctxt
      rollup
      refuter
      refuter_commitment
  in
  let*@ _, _, ctxt, _ =
    Sc_rollup_stake_storage.publish_commitment
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
  let*!@ res =
    R.start_game
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

(* Build a repr-level [Sc_rollup_proof_repr.t] whose PVM part is a genuine Arith
   PVM step (so the proof verifier does not raise on the PVM side — that would be
   a separate failure mode) and whose input proof is [input_proof].

   [Alpha_context.Sc_rollup.Proof] and [Sc_rollup_proof_repr] are the same module
   underneath (see [alpha_context.ml]); only the .mli seals the types apart.
   Carry the value across that seal through the byte-identical wire encoding so
   it can feed the repr-level [G.Proof] consumed by [R.game_move] and by the
   block-validation plugin. *)
let build_repr_proof input_proof =
  let open Lwt_result_wrap_syntax in
  let pvm_ctxt = Arith_pvm.make_empty_context () in
  let empty = Arith_pvm.make_empty_state () in
  let*! state = Arith_pvm.initial_state ~empty in
  let*! state = Arith_pvm.eval state in
  let is_reveal_enabled = Sc_rollup_helpers.is_reveal_enabled_default in
  let*! pvm_step =
    Arith_pvm.produce_proof pvm_ctxt ~is_reveal_enabled None state
  in
  let pvm_step = WithExceptions.Result.get_ok ~loc:__LOC__ pvm_step in
  let pvm_step =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Alpha_context.Sc_rollup.Proof.serialize_pvm_step
         ~pvm:(module Arith_pvm)
         pvm_step
  in
  let alpha_proof = Alpha_context.Sc_rollup.Proof.{pvm_step; input_proof} in
  return
    (Data_encoding.Binary.of_bytes_exn Sc_rollup_proof_repr.encoding
    @@ Data_encoding.Binary.to_bytes_exn
         Alpha_context.Sc_rollup.Proof.encoding
         alpha_proof)

(* Build a [Proof] step whose PVM part is genuine (so the proof verifier does
   not raise — that would be a separate failure mode) while the inbox proof is
   intentionally bogus. The block-validation plugin's multi-tick check never
   inspects the proof's contents — only the chosen section's tick distance — so
   any well-typed proof value works here. Shared by the plugin tests. *)
let build_adversarial_proof () =
  let inbox_proof =
    Alpha_context.Sc_rollup.Inbox.Internal_for_tests.serialized_proof_of_string
      "I am the big bad wolf"
  in
  let input_proof =
    Alpha_context.Sc_rollup.Proof.Inbox_proof
      {
        level = Alpha_context.Raw_level.root;
        message_counter = Z.zero;
        proof = inbox_proof;
      }
  in
  build_repr_proof (Some input_proof)

(* Build a [Proof] step with no defect of its own: the PVM step is genuine and
   [input_proof = None] matches the [No_input_required] request of its start
   state, so [Sc_rollup_proof_repr.valid] returns [Ok]. Unlike
   [build_adversarial_proof], it lets
   [test_proof_on_missing_start_state_forces_indefensible_final_move] exercise a
   single defect: the [None] agreed start state of the section played on.

   Reaching [No_input_required]: [initial_state] is [Halted], the first [eval]
   boots the PVM into [Waiting_for_input_message], [set_input] hands it a
   message, and the second [eval] moves it to [Parsing] — a tick consuming no
   input. *)
let build_well_formed_proof () =
  let open Lwt_result_wrap_syntax in
  let pvm_ctxt = Arith_pvm.make_empty_context () in
  let empty = Arith_pvm.make_empty_state () in
  let is_reveal_enabled = Sc_rollup_helpers.is_reveal_enabled_default in
  let*! state = Arith_pvm.initial_state ~empty in
  let*! state = Arith_pvm.eval state in
  let*! state =
    Arith_pvm.set_input (Sc_rollup_helpers.make_external_input "1") state
  in
  let*! state = Arith_pvm.eval state in
  (* Guard the construction above: were the arith PVM to stop being input-free
     at this tick, [input_proof = None] would no longer be well-formed and this
     helper would silently reintroduce a second defect. *)
  let*! input_request = Arith_pvm.is_input_state ~is_reveal_enabled state in
  let* () =
    match input_request with
    | Alpha_context.Sc_rollup.No_input_required -> return_unit
    | _ ->
        Test.fail
          "build_well_formed_proof: expected the arith PVM to require no input \
           here, so that a proof with [input_proof = None] is well-formed"
  in
  let*! pvm_step =
    Arith_pvm.produce_proof pvm_ctxt ~is_reveal_enabled None state
  in
  let pvm_step = WithExceptions.Result.get_ok ~loc:__LOC__ pvm_step in
  let pvm_step =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Alpha_context.Sc_rollup.Proof.serialize_pvm_step
         ~pvm:(module Arith_pvm)
         pvm_step
  in
  let alpha_proof =
    Alpha_context.Sc_rollup.Proof.{pvm_step; input_proof = None}
  in
  (* Same .mli-seal crossing as in [build_adversarial_proof]. *)
  return
    (Data_encoding.Binary.of_bytes_exn Sc_rollup_proof_repr.encoding
    @@ Data_encoding.Binary.to_bytes_exn
         Alpha_context.Sc_rollup.Proof.encoding
         alpha_proof)

(* Reconstruct the [Sc_rollup_refute] operation carrying [repr_refutation]
   (given at the [Sc_rollup_game_repr] level). *)
let mk_refute_op ~source ~opponent ~rollup ~repr_refutation :
    Alpha_context.packed_operation =
  let contents =
    Alpha_context.Manager_operation
      {
        source;
        fee = Alpha_context.Tez.zero;
        counter = Alpha_context.Manager_counter.Internal_for_tests.of_int 0;
        operation =
          Alpha_context.Sc_rollup_refute
            {
              rollup;
              opponent;
              refutation =
                (* Carry the refutation across the .mli seal between
                   [Sc_rollup_game_repr] and [Alpha_context.Sc_rollup.Game]
                   through the byte-identical wire encoding (same trick used
                   for the proof). *)
                Data_encoding.Binary.of_bytes_exn
                  Alpha_context.Sc_rollup.Game.refutation_encoding
                @@ Data_encoding.Binary.to_bytes_exn
                     Sc_rollup_game_repr.refutation_encoding
                     repr_refutation;
            };
        gas_limit = Alpha_context.Gas.Arith.zero;
        storage_limit = Z.zero;
      }
  in
  let shell = Tezos_base.Operation.{branch = Block_hash.zero} in
  let protocol_data =
    Alpha_context.Operation_data {contents = Single contents; signature = None}
  in
  {shell; protocol_data}

(* Run the block-validation plugin against [ctxt_before] — the start-of-block
   context a validator sees — threading the plugin state through [ops] as if
   they were included in the same block. Returns the plugin's verdict on the
   first rejected operation, [Ok _] if all pass. *)
let run_block_validation_plugin_ops ~ctxt_before ops =
  let open Lwt_result_wrap_syntax in
  let chain_id = Chain_id.zero in
  (* The repr-level game functions thread a [Raw_context.t]; the plugin and
     [Validate] take [Alpha_context.t]. The .mli seals these apart on purpose
     ([raw_context.mli]: "Alpha_context.t is actually implemented as
     Raw_context.t [...] not exposed"). The legitimate bridge is to recover the
     underlying disk [Context.t] (which carries the persisted game storage) and
     re-prepare an [Alpha_context.t] from it. *)
  let disk_ctxt = Raw_context.recover ctxt_before in
  let level =
    Raw_level_repr.to_int32 (Raw_context.current_level ctxt_before).level
  in
  let predecessor_timestamp = Raw_context.predecessor_timestamp ctxt_before in
  let timestamp = Raw_context.current_timestamp ctxt_before in
  let*@ alpha_ctxt, _, _ =
    Alpha_context.prepare disk_ctxt ~level ~predecessor_timestamp ~timestamp
  in
  let level = Alpha_context.Level.current alpha_ctxt in
  let validation_state =
    Validate.begin_partial_construction
      alpha_ctxt
      chain_id
      ~predecessor_level:level
      ~predecessor_round:Alpha_context.Round.zero
  in
  let plugin_state =
    Block_validation.init_block_validation_state validation_state
  in
  List.fold_left_es Block_validation.check_block_operation plugin_state ops

(* Single-operation wrapper: run the plugin on one [Move {choice; Proof}]. *)
let run_block_validation_plugin ~ctxt_before ~source ~opponent ~rollup ~choice
    ~proof =
  let repr_refutation =
    Sc_rollup_game_repr.Move {choice; step = G.Proof proof}
  in
  run_block_validation_plugin_ops
    ~ctxt_before
    [mk_refute_op ~source ~opponent ~rollup ~repr_refutation]

(* E2E protocol-unit reproduction.

   A [Proof] move submitted while the game is still [Dissecting] over a chosen
   section whose tick distance is > 1 must be REJECTED ([Proof_unexpected_section_size]).
   The current protocol instead collapses that error to [false] and stores a
   [Final_move], which is indefensible for the honest opponent: a [Final_move]
   timeout resolves to [Draw], and [Draw] removes BOTH stakes — slashing the
   honest defender.

   This test drives the full chain on the real on-chain entry points
   ([R.start_game], [R.game_move], [R.timeout], [R.apply_game_result]):
     1. start a game whose initial dissection is the wide section [0; 10000];
     2. the refuter plays [Proof] with choice=0 (section [0;10000]);
     3. assert the move is ACCEPTED and the stored state becomes [Final_move]
        (the bug), instead of failing with [Proof_unexpected_section_size];
     4. let the [Final_move] time out -> [Draw];
     5. apply the [Draw] -> assert the honest defender is slashed. *)
let test_multitick_proof_forces_indefensible_final_move () =
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
  (* The honest defender is staked before the game ends. *)
  let*@ ctxt, defender_staked_before =
    Sc_rollup_staker_index_storage.is_staker ctxt rollup defender
  in
  let* () = Assert.equal_bool ~loc:__LOC__ defender_staked_before true in
  (* Open the game. Its initial dissection is the single wide first section
     [parent@0; child@10000; None@10001]: choice=0 selects [0; 10000]. *)
  let*@ ctxt =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  let* proof = build_adversarial_proof () in
  let choice = Sc_rollup_tick_repr.initial in
  let step = G.Proof proof in
  (* Capture the context BEFORE the move is applied — at this point the
     game is still in [Dissecting], which is exactly the state a baker sees at
     the start of the block that would include this operation. This is the
     context the block validation plugin runs against. *)
  let ctxt_before_move = ctxt in
  let*@ game_result, ctxt =
    R.game_move ctxt rollup ~player:refuter ~opponent:defender ~choice ~step
  in
  (* BUG #1: the out-of-phase multi-tick proof is ACCEPTED — the move does not
     fail with [Proof_unexpected_section_size]; the game merely advances. *)
  let* () = Assert.is_none ~loc:__LOC__ ~pp:G.pp_game_result game_result in
  (* BUG #2: the stored game state is now [Final_move] over the wide section. *)
  let index = G.Index.make refuter defender in
  let*@ ctxt, game_opt = R.find_game ctxt rollup index in
  let* () =
    match game_opt with
    | Some {game_state = G.Final_move _; _} -> return_unit
    | Some {game_state = G.Dissecting _; _} ->
        Stdlib.failwith
          "bug not reproduced: state is still Dissecting (move rejected?)"
    | None -> Stdlib.failwith "game disappeared after the move"
  in
  (* PLUGIN CHECK: the block validation plugin must reject this
     operation, run against the start-of-block context [ctxt_before_move]. *)
  let*! plugin_res =
    run_block_validation_plugin
      ~ctxt_before:ctxt_before_move
      ~source:refuter
      ~opponent:defender
      ~rollup
      ~choice
      ~proof
  in
  (match plugin_res with
  | Ok _ ->
      Stdlib.failwith "plugin FAILED: the move was NOT rejected by the plugin"
  | Error _ -> ()) ;
  (* IMPACT: nobody can defend a [Final_move] over a multi-tick section. Let it
     time out and confirm the protocol returns [Draw]. *)
  let timeout_period =
    Constants_storage.sc_rollup_timeout_period_in_blocks ctxt
  in
  let ctxt =
    Raw_context.Internal_for_tests.add_level ctxt (timeout_period + 1)
  in
  let*@ game_result, ctxt = R.timeout ctxt rollup index in
  let* () =
    match game_result with
    | G.Draw -> return_unit
    | G.Loser _ -> Stdlib.failwith "expected Draw, got Loser"
  in
  (* [Draw] removes both stakers — the honest defender is slashed. *)
  let*@ _status, ctxt, _balance_updates =
    R.apply_game_result ctxt rollup index game_result
  in
  let*@ ctxt, defender_staked_after =
    Sc_rollup_staker_index_storage.is_staker ctxt rollup defender
  in
  ignore ctxt ;
  Assert.equal_bool ~loc:__LOC__ defender_staked_after false

(* Companion test: a [Proof] move whose [choice] is the very LAST tick of
   the dissection names no section — the final tick is the closing boundary, so
   [find_choice] has no [next] chunk after it. Unlike the multi-tick case
   (where [find_choice] succeeds and the missing distance-one check lets the bad
   proof through), the protocol rejects this move outright with
   [Dissection_choice_not_found]. This drives the real on-chain entry point
   [R.game_move] rather than the shell plugin. *)
let test_proof_choice_on_last_tick_rejected_by_protocol () =
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
  (* Open the game: its initial dissection is the wide first section
     [parent@0; child@10000; None@10001], so the last (boundary) tick is 10001. *)
  let*@ ctxt =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  (* Read the freshly-opened (still [Dissecting]) game and take the LAST tick of
     its dissection as the [choice]. *)
  let index = G.Index.make refuter defender in
  let*@ ctxt, game_opt = R.find_game ctxt rollup index in
  let* last_tick =
    match game_opt with
    | Some {game_state = G.Dissecting {dissection; _}; _} -> (
        match List.last_opt dissection with
        | Some last -> return last.D.tick
        | None -> Stdlib.failwith "last-tick: empty dissection")
    | _ ->
        Stdlib.failwith
          "last-tick: expected a Dissecting game right after start_game"
  in
  let* proof = build_adversarial_proof () in
  let step = G.Proof proof in
  (* The protocol must reject the choice before it ever inspects the proof:
     [find_choice] fails on the final boundary tick. *)
  let*!@ res =
    R.game_move
      ctxt
      rollup
      ~player:refuter
      ~opponent:defender
      ~step
      ~choice:last_tick
  in
  assert_fails_with_f ~__LOC__ (Lwt.return res) (function
    | G.Dissection_choice_not_found tick ->
        Sc_rollup_tick_repr.(tick = last_tick)
    | _ -> false)

(* The block-validation plugin checks refutation moves against the context at
   the START of the block, which is not updated as operations are applied. A
   dissection move followed, in the same block, by a Proof move on the same
   game would therefore be checked against a stale dissection and slip through
   (its choice does not appear in the start-of-block dissection). The plugin
   closes this hole by allowing at most one operation per refutation game per
   block. This test checks:
   1. a lone Proof whose choice is not in the start-of-block dissection passes
      the plugin (the protocol itself rejects it at application);
   2. the same Proof is REJECTED when preceded, in the same block, by another
      refutation operation on the same game;
   3. operations on distinct games in the same block are not affected. *)
let test_at_most_one_operation_per_game_per_block () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt,
         rollup,
         refuter,
         defender,
         staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let*@ ctxt =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  (* Start-of-block context: the game is [Dissecting] over the wide section
     [parent@0; child@10000; None@10001]. *)
  let ctxt_before = ctxt in
  (* First operation: the refuter plays a dissection move. Its contents are
     irrelevant to the plugin — only the fact that it targets the game. *)
  let op_dissection =
    mk_refute_op
      ~source:refuter
      ~opponent:defender
      ~rollup
      ~repr_refutation:
        (G.Move
           {
             choice = Sc_rollup_tick_repr.initial;
             step = G.Dissection (init_dissection ~size:4 (hash_string "foo"));
           })
  in
  (* Second operation: the defender answers with a Proof whose choice (5000)
     only exists in the refreshed dissection — it is not a chunk tick of the
     start-of-block one, so the stale-context proof check cannot see it. *)
  let* proof = build_adversarial_proof () in
  let op_proof =
    mk_refute_op
      ~source:defender
      ~opponent:refuter
      ~rollup
      ~repr_refutation:
        (G.Move {choice = tick_of_int_exn 5000; step = G.Proof proof})
  in
  (* 1. Alone, the Proof passes the plugin: with no earlier operation on the
     game in the block, the start-of-block state is up to date and the
     protocol itself rejects the unknown choice at application. *)
  let*! res = run_block_validation_plugin_ops ~ctxt_before [op_proof] in
  let* () =
    match res with
    | Ok _ -> return_unit
    | Error _ ->
        Stdlib.failwith
          "one-op-per-game: a lone Proof should pass the plugin (the protocol \
           rejects it at application)"
  in
  (* 2. Preceded by another operation on the same game, it must be rejected. *)
  let*! res =
    run_block_validation_plugin_ops ~ctxt_before [op_dissection; op_proof]
  in
  let* () =
    match res with
    | Ok _ ->
        Stdlib.failwith
          "one-op-per-game: second operation on the same game in the same \
           block was NOT rejected by the plugin"
    | Error _ -> return_unit
  in
  (* 3. An operation on a DIFFERENT game in the same block is unaffected. *)
  let op_other_game =
    mk_refute_op
      ~source:refuter
      ~opponent:staker3
      ~rollup
      ~repr_refutation:
        (G.Move {choice = tick_of_int_exn 5000; step = G.Proof proof})
  in
  let*! res =
    run_block_validation_plugin_ops ~ctxt_before [op_dissection; op_other_game]
  in
  match res with
  | Ok _ -> return_unit
  | Error _ ->
      Stdlib.failwith
        "one-op-per-game: operation on a distinct game was wrongly rejected"

(* End-to-end reproduction of the inbox level-confusion soundness bug showing
   that an attacker genuinely WINS a refutation game (not merely that the move
   is accepted).

   [Sc_rollup_inbox_repr.verify_proof] (hence [Sc_rollup_proof_repr.valid])
   never binds the level actually PROVEN by an inbox inclusion proof to the
   [level] CLAIMED in the [Inbox_proof]. An attacker can therefore submit a
   genuine, unmodified inclusion proof for a real message at [attacker_level]
   while CLAIMING [victim_level] — the reconstructed input keeps the claimed
   level but carries the attacker level's payload. Fed to a single disputed
   PVM tick, this produces a stop state that differs from the honest defender's
   commitment, so the refutation succeeds and the honest defender loses.

   Construction (no dissection-narrowing loop needed: the disputed commitment
   covers exactly ONE PVM tick, so the opening dissection's first section
   [0; 1] is already distance-one):
     1. install a genuine multi-level inbox as the on-chain inbox;
     2. run the real Arith PVM over that inbox up to a state [s0] requesting
        [First_after (victim_level, 2)] (i.e. right after consuming the victim
        level's first external message);
     3. publish a parent commitment whose state is [s0], and, sharing it as
        predecessor, an HONEST defender commitment of exactly one tick whose
        state is the honest result of consuming the victim level's next message
        [(victim_level, 3)], plus a differing refuter commitment;
     4. open the game and build that [Proof] — a genuine inclusion proof of
        the ATTACKER level's message [(attacker_level, 3)] wrapped in an
        [Inbox_proof] CLAIMING [victim_level];
     5. assert the protocol consensus is STILL unsound: fed this proof
        directly, [R.game_move] accepts it and declares the honest defender
        [Loser] — the expected, unfixed protocol behaviour;
     6. assert the block-validation plugin REJECTS the same operation with
        [Sc_rollup_inbox_proof_claimed_level_mismatch] carrying
        [claimed_level = victim_level] and [proven_level = attacker_level],
        showing the filter blocks the attack consensus alone lets win. *)
let test_inbox_proof_level_confusion () =
  let open Lwt_result_wrap_syntax in
  let open Alpha_context in
  let* ctxt, rollup, genesis_hash, refuter, defender, _staker3 =
    T.originate_rollup_and_deposit_with_three_stakers ()
  in
  (* The rollup's genuine metadata. The disputed inbox levels must lie strictly
     above the origination level, otherwise [cut_at_level] drops the input. *)
  let*@ ctxt, metadata_repr = Sc_rollup_storage.get_metadata ctxt rollup in
  let origination_level_i =
    Int32.to_int
      (Raw_level_repr.to_int32
         metadata_repr.Sc_rollup_metadata_repr.origination_level)
  in
  let victim_level_i = origination_level_i + 2 in
  let attacker_level_i = origination_level_i + 3 in
  let victim_level = Raw_level.of_int32_exn (Int32.of_int victim_level_i) in
  let attacker_level = Raw_level.of_int32_exn (Int32.of_int attacker_level_i) in
  let metadata =
    Sc_rollup.Metadata.
      {
        address = rollup;
        origination_level =
          Raw_level.of_int32_exn (Int32.of_int origination_level_i);
      }
  in
  (* Build a genuine multi-level inbox: levels [1 .. attacker_level], two
     external messages each. [inbox_creation_level] must be [root] so that the
     [first_block] flag lines up between the PVM inputs and the inbox
     merkelisation (only absolute level 1 is a first block). All disputed levels
     are [>= 2], hence regular levels whose external messages sit at indices 2
     and 3. *)
  let payloads_for_levels =
    Stdlib.List.init attacker_level_i (fun i ->
        let k = i + 1 in
        Sc_rollup_helpers.wrap_messages
          (Raw_level.of_int32_exn (Int32.of_int k))
          [Printf.sprintf "l%d-m0" k; Printf.sprintf "l%d-m1" k])
  in
  let*? node_inbox, proto_inbox =
    Sc_rollup_helpers.construct_node_and_protocol_inbox
      ~inbox_creation_level:Raw_level.root
      payloads_for_levels
  in
  let inputs_per_levels =
    List.map (fun p -> p.Sc_rollup_helpers.inputs) payloads_for_levels
  in
  let is_reveal_enabled = Sc_rollup_helpers.is_reveal_enabled_default in
  (* [s0]: the first PVM state requesting [First_after (victim_level, 2)]. We
     locate it by evaluating the real Arith PVM with increasing fuel until the
     input request matches — the honest agreed prestate of the disputed tick. *)
  let victim_counter = Z.of_int 2 in
  let state_is_s0 state =
    let open Lwt_syntax in
    let* req = Arith_pvm.is_input_state ~is_reveal_enabled state in
    match req with
    | Sc_rollup.First_after (l, n) ->
        return (Raw_level.equal l victim_level && Z.equal n victim_counter)
    | _ -> return false
  in
  let rec find_s0 fuel =
    if fuel > 2000 then failwith "level-confusion: [s0] not reached"
    else
      let*! r =
        Arith_pvm_eval.eval_inputs_from_initial_state
          ~metadata
          ~fuel
          inputs_per_levels
      in
      match r with
      | Error _ -> find_s0 (fuel + 1)
      | Ok (state, _tick, _states) ->
          let*! is_s0 = state_is_s0 state in
          if is_s0 then return state else find_s0 (fuel + 1)
  in
  let* s0 = find_s0 1 in
  let Sc_rollup_helpers.Node_inbox.{inbox = local_inbox; _} = node_inbox in
  let snapshot = Sc_rollup.Inbox.take_snapshot local_inbox in
  (* Genuine inbox proof + input for the attacker's message at
     [(attacker_level, 3)]. [valid] reconstructs the input by verifying this
     proof at index [succ 2 = 3]: the inclusion proof genuinely proves
     [attacker_level], but the returned message is (unsoundly) tagged with the
     claimed [victim_level]. *)
  let* attacker_inbox_proof, attacker_input =
    Sc_rollup_helpers.Node_inbox.produce_proof
      node_inbox
      snapshot
      (attacker_level, Z.of_int 3)
  in
  (* The honest next input at [(victim_level, 3)] an honest defender consumed. *)
  let* _honest_inbox_proof, honest_input =
    Sc_rollup_helpers.Node_inbox.produce_proof
      node_inbox
      snapshot
      (victim_level, Z.of_int 3)
  in
  let honest_input =
    match honest_input with
    | Some m -> Sc_rollup.Inbox_message m
    | None -> assert false
  in
  (* Re-tag the genuine attacker-level message as [victim_level]: this is
     exactly what [valid] reconstructs from the level-confused [Inbox_proof]. *)
  let confused_input =
    match attacker_input with
    | Some {Sc_rollup.message_counter; payload; _} ->
        Sc_rollup.Inbox_message
          {inbox_level = victim_level; message_counter; payload}
    | None -> assert false
  in
  let pvm_ctxt = Arith_pvm.make_empty_context () in
  (* Honest single-tick step from [s0]: its stop state is what an honest
     defender commits. *)
  let*! honest_step =
    Arith_pvm.produce_proof pvm_ctxt ~is_reveal_enabled (Some honest_input) s0
  in
  let honest_step = WithExceptions.Result.get_ok ~loc:__LOC__ honest_step in
  let honest_stop = Arith_pvm.proof_stop_state honest_step in
  (* Attacker single-tick step from [s0] consuming the wrong-level payload. *)
  let*! attacker_step =
    Arith_pvm.produce_proof pvm_ctxt ~is_reveal_enabled (Some confused_input) s0
  in
  let attacker_step = WithExceptions.Result.get_ok ~loc:__LOC__ attacker_step in
  let s0_hash = Arith_pvm.proof_start_state attacker_step in
  let attacker_stop = Arith_pvm.proof_stop_state attacker_step in
  (* The attack genuinely refutes: feeding the wrong-level payload yields a
     different stop state than the honest one. *)
  let* () =
    Assert.equal_bool
      ~loc:__LOC__
      (Sc_rollup.State_hash.equal attacker_stop honest_stop)
      false
  in
  (* Publish the commitments carrying these REAL PVM state hashes. The disputed
     defender commitment covers exactly one tick, so the game opens directly on
     a distance-one section. *)
  let level_of = T.valid_inbox_level ctxt in
  let parent_commit =
    Commitment_repr.
      {
        predecessor = genesis_hash;
        inbox_level = level_of 1l;
        number_of_ticks = T.number_of_ticks_exn 1L;
        compressed_state = s0_hash;
      }
  in
  let*@ parent_hash, _, ctxt =
    T.advance_level_n_refine_stake ctxt rollup defender parent_commit
  in
  let defender_commit =
    Commitment_repr.
      {
        predecessor = parent_hash;
        inbox_level = level_of 2l;
        number_of_ticks = T.number_of_ticks_exn 1L;
        compressed_state = honest_stop;
      }
  in
  let refuter_commit =
    Commitment_repr.
      {
        predecessor = parent_hash;
        inbox_level = level_of 2l;
        number_of_ticks = T.number_of_ticks_exn 1L;
        compressed_state = hash_string "refuter-genuine-attack";
      }
  in
  let ctxt = T.advance_level_for_commitment ctxt defender_commit in
  let*@ _, _, ctxt, _ =
    Sc_rollup_stake_storage.publish_commitment
      ctxt
      rollup
      defender
      defender_commit
  in
  let*@ _, _, ctxt, _ =
    Sc_rollup_stake_storage.publish_commitment
      ctxt
      rollup
      refuter
      refuter_commit
  in
  let defender_commitment_hash =
    Sc_rollup_commitment_repr.hash_uncarbonated defender_commit
  in
  let refuter_commitment_hash =
    Sc_rollup_commitment_repr.hash_uncarbonated refuter_commit
  in
  (* Install the genuine multi-level inbox as the on-chain inbox so that the
     game's [inbox_snapshot] is a real multi-level inbox (the precondition for
     an inbox inclusion proof to verify). Carry [proto_inbox] across the .mli
     seal through the wire encoding. *)
  let inbox_repr =
    Data_encoding.Binary.of_bytes_exn Sc_rollup_inbox_repr.encoding
    @@ Data_encoding.Binary.to_bytes_exn Sc_rollup.Inbox.encoding proto_inbox
  in
  let*! ctxt = Storage.Sc_rollup.Inbox.add ctxt inbox_repr in
  (* Open the game. Its opening dissection is
     [(s0, 0); (honest_stop, 1); (None, 2)], so [choice = tick 0] selects the
     already distance-one section [0; 1]. *)
  let*@ ctxt =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  (* The level-confused [Inbox_proof]: a genuine inclusion proof of the message
     at [attacker_level] wrapped as if it were at [victim_level]. *)
  let input_proof =
    Sc_rollup.Proof.Inbox_proof
      {
        level = victim_level;
        message_counter = victim_counter;
        proof = Sc_rollup.Inbox.to_serialized_proof attacker_inbox_proof;
      }
  in
  let pvm_step =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Sc_rollup.Proof.serialize_pvm_step ~pvm:(module Arith_pvm) attacker_step
  in
  let alpha_proof =
    Sc_rollup.Proof.{pvm_step; input_proof = Some input_proof}
  in
  (* Carry the proof across the [Alpha_context.Sc_rollup.Proof] / repr seal. *)
  let proof =
    Data_encoding.Binary.of_bytes_exn Sc_rollup_proof_repr.encoding
    @@ Data_encoding.Binary.to_bytes_exn Sc_rollup.Proof.encoding alpha_proof
  in
  (* Consensus is still unsound: fed this proof directly, [R.game_move]
     accepts it and the honest defender LOSES the game. This is the expected
     (unfixed) protocol behaviour the plugin filter below shields against. We
     run it on [ctxt] and discard the resulting context so the plugin check
     replays the same pre-move state. *)
  let*@ game_result, _ctxt =
    R.game_move
      ctxt
      rollup
      ~player:refuter
      ~opponent:defender
      ~choice:Sc_rollup_tick_repr.initial
      ~step:(G.Proof proof)
  in
  let* () =
    match game_result with
    | Some (G.Loser {loser; _}) ->
        Assert.equal_bool
          ~loc:__LOC__
          (Sc_rollup_repr.Staker.equal loser defender)
          true
    | Some G.Draw ->
        Test.fail
          "level-confusion: the attack resolved to a Draw instead of a win"
    | None ->
        Test.fail
          "level-confusion: the wrong-level proof did not win the game \
           (game_move returned None instead of Loser defender)"
  in
  (* The plugin must reject the operation: the proof proves level 3
     but claims level 2. *)
  let*! plugin_res =
    run_block_validation_plugin
      ~ctxt_before:ctxt
      ~source:refuter
      ~opponent:defender
      ~rollup
      ~choice:Sc_rollup_tick_repr.initial
      ~proof
  in
  match plugin_res with
  | Ok _ ->
      Test.fail
        "level-confusion: the plugin accepted an inbox proof whose proven \
         level (3) does not match the claimed level (2)"
  | Error trace ->
      if
        List.exists
          (function
            | Environment.Ecoproto_error
                (Block_validation.Sc_rollup_inbox_proof_claimed_level_mismatch
                   {claimed_level; proven_level}) ->
                Raw_level.equal claimed_level victim_level
                && Raw_level.equal proven_level attacker_level
            | _ -> false)
          trace
      then return_unit
      else
        Test.fail
          "level-confusion: the plugin rejected the operation but not with \
           Sc_rollup_inbox_proof_claimed_level_mismatch {claimed = 2; proven = \
           3}"

(* Companion to [test_multitick_proof_forces_indefensible_final_move] for the
   OTHER indefensible-[Final_move] shape: a [Proof] played on a distance-one
   section whose agreed START state is absent ([None]).

   [Sc_rollup_dissection_chunk_repr.default_check] only forbids a [None] chunk
   followed by a [Some] one, so a dishonest player may publish a dissection with
   two consecutive [None] chunks one tick apart: the section between them passes
   the multi-tick check, yet no PVM proof can start from [None]. Both players
   fail [validity_first_final_move], the protocol anchors a [Final_move] on the
   [None] chunk, and the game can only resolve as a [Draw] — burning the honest
   player's bond.

   Through the real on-chain entry points:
     1. start a game (opening dissection [foo@0; child@10000; None@10001]);
     2. the refuter dissects [0; 10000] validly, with ticks 0, 1, 2 and [None]
        states from tick 1 on, making [1; 2] a distance-one section with a
        [None] start state;
     3. assert the protocol ACCEPTS the defender's [Proof] there and traps the
        game in a [Final_move] on the [None] chunk;
     4. assert the block-validation plugin REJECTS the same operation with
        [Sc_rollup_proof_on_missing_start_state_during_dissecting 1]. *)
let test_proof_on_missing_start_state_forces_indefensible_final_move () =
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
  let*@ ctxt =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  (* The refuter's dissection of the opening section [0; 10000]: its head carries
     the agreed start state ([hash_string "foo"]), every other chunk is [None] (a
     valid trailing run, its last element differing from the disputed stop
     state). Ticks 0, 1, 2 then an even spread, so no section exceeds the maximum
     size ([distance / 2]). *)
  let size =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in
  let stop_tick = 10_000 in
  let tick_step = (stop_tick - 2) / (size - 2) in
  let dissection =
    Stdlib.List.init (size + 1) (fun i ->
        mk_dissection_chunk
        @@
        if i = 0 then (Some (hash_string "foo"), tick_of_int_exn 0)
        else if i = size then (None, tick_of_int_exn stop_tick)
        else if i <= 2 then (None, tick_of_int_exn i)
        else (None, tick_of_int_exn (2 + ((i - 2) * tick_step))))
  in
  let*@ game_result, ctxt =
    R.game_move
      ctxt
      rollup
      ~player:refuter
      ~opponent:defender
      ~choice:Sc_rollup_tick_repr.initial
      ~step:(G.Dissection dissection)
  in
  (* [None] means the move was applied and the game CONTINUES. *)
  let* () = Assert.is_none ~loc:__LOC__ ~pp:G.pp_game_result game_result in
  (* The honest defender must now answer on the distance-one section [1; 2],
     where the game rules demand a [Proof]. *)
  let* proof = build_well_formed_proof () in
  let choice = tick_of_int_exn 1 in
  (* What a baker sees before including the [Proof]: the game still
     [Dissecting]. *)
  let ctxt_before_move = ctxt in
  (* PROTOCOL STEP: consensus ACCEPTS the move — nothing in the protocol forbids
     a [Proof] on a section whose agreed start state is [None], so only the
     plugin keeps it out of blocks. *)
  let*@ game_result, ctxt =
    R.game_move
      ctxt
      rollup
      ~player:defender
      ~opponent:refuter
      ~choice
      ~step:(G.Proof proof)
  in
  let* () =
    (* Accepted, yet it resolves NOTHING: the proof cannot even be checked (no
       PVM proof starts from [None]), so instead of losing, the defender is
       trapped in a game that can only end in a [Draw]. *)
    Assert.is_none ~loc:__LOC__ ~pp:G.pp_game_result game_result
  in
  let index = G.Index.make refuter defender in
  let*@ _ctxt, game_opt = R.find_game ctxt rollup index in
  let* () =
    match game_opt with
    | Some {game_state = G.Final_move {agreed_start_chunk; _}; _} ->
        (* The BUGGY behaviour this test documents: the [Final_move] is anchored
           on the [None] chunk, so the refuter's answer fails the same
           start-state check. Neither player can prove anything, the game can
           only end in a [Draw], and both bonds are slashed — the honest
           defender is punished for the refuter's dissection. *)
        Assert.equal_bool
          ~loc:__LOC__
          (Option.is_none agreed_start_chunk.D.state_hash)
          true
    | Some {game_state = G.Dissecting _; _} ->
        (* The behaviour consensus SHOULD have: refuse the move (as it already
           does for a [Proof] whose [choice] names no section) instead of
           storing a [Final_move] nobody can answer. Reaching this branch means
           the protocol was fixed — a WELCOME failure: update the test, not the
           protocol. *)
        Test.fail
          "missing-start-state: the accepted Proof no longer moves the game to \
           Final_move (still Dissecting): consensus behaviour changed for the \
           better, so this test's premise is stale and needs revisiting"
    | None -> Test.fail "missing-start-state: game disappeared after the move"
  in
  (* PLUGIN CHECK: the same operation, against the start-of-block context, must
     be rejected with the tick of the [None] start chunk. *)
  let*! plugin_res =
    run_block_validation_plugin
      ~ctxt_before:ctxt_before_move
      ~source:defender
      ~opponent:refuter
      ~rollup
      ~choice
      ~proof
  in
  match plugin_res with
  | Ok _ ->
      Test.fail
        "missing-start-state: the plugin accepted a Proof on a distance-one \
         section whose start state is None"
  | Error trace ->
      if
        List.exists
          (function
            | Environment.Ecoproto_error
                (Block_validation
                 .Sc_rollup_proof_on_missing_start_state_during_dissecting
                   tick) ->
                (* The tick tells the baker WHICH chunk is at fault; here the
                   chosen section is [1; 2], so it must be 1 — not, say, the
                   stop chunk at tick 2. *)
                Z.equal tick Z.one
            | _ -> false)
          trace
      then return_unit
      else
        Test.fail
          "missing-start-state: the plugin rejected the operation but not with \
           Sc_rollup_proof_on_missing_start_state_during_dissecting 1"

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
    Tztest.tztest
      "multi-tick Proof in Dissecting forces an indefensible Final_move (Draw \
       slashes the honest player)."
      `Quick
      test_multitick_proof_forces_indefensible_final_move;
    Tztest.tztest
      "A Proof whose choice is the last (boundary) tick is rejected by the \
       protocol (Dissection_choice_not_found)."
      `Quick
      test_proof_choice_on_last_tick_rejected_by_protocol;
    Tztest.tztest
      "The plugin allows at most one operation per refutation game per block."
      `Quick
      test_at_most_one_operation_per_game_per_block;
    Tztest.tztest
      "An inbox proof whose proven level does not match the claimed level lets \
       the attacker win the refutation game (level-confusion soundness bug)."
      `Quick
      test_inbox_proof_level_confusion;
    Tztest.tztest
      "A Proof on a distance-one section whose agreed start state is absent \
       forces an indefensible Final_move."
      `Quick
      test_proof_on_missing_start_state_forces_indefensible_final_move;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("sc rollup game", tests)]
  |> Lwt_main.run
