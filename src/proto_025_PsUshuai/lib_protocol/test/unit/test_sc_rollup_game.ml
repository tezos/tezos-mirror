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
    Invocation: dune exec src/proto_025_PsUshuai/lib_protocol/test/unit/main.exe \
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

(* ------------------------------------------------------------------------ *)
(* The plugin checks refutation moves against the context at the START of the
   block, which is not updated as operations are applied. A check on a game is
   therefore only meaningful while no earlier operation in the block has moved
   that game.

   The rule that buys it, with no state threaded through the block: a [Move] is
   accepted only from the player the game expects at the start of the block.
   Every move [play] accepts flips the turn, so once one has applied, every
   later [Move] on that game fails the protocol's own [Sc_rollup_wrong_turn]
   check before reaching [play] — at most one of them can change anything, and
   it is the one the start-of-block context describes.

   Refusing the others costs them nothing they cannot recover. A move played
   out of turn can only fail at APPLICATION ([game_move]'s
   [Sc_rollup_wrong_turn], [get_game]'s [Sc_rollup_no_game]), so its sender
   loses one block out of the [timeout_period_in_blocks] their clock is
   measured in, and may inject it again once the turn is theirs.

   [Start] is left alone. It has no turn to be out of, no check here reads the
   state for it, and before the game exists both players may legitimately open
   it — the one window in which two operations on a game genuinely compete, and
   the one the plugin must not arbitrate.

   The tests below are in three groups: what the rule must refuse, what it must
   not, and the checks it keeps sound. Each one that turns on a refusal first
   establishes at the protocol level what the refused operation could have
   done. Inclusion is checked by replaying the rule the baker applies to the
   plugin's verdict — an operation the plugin refuses is left out of the block
   ([lib_delegate/operation_selection.ml]) — rather than by asserting the
   verdict itself. No baker runs. *)

(* The [Sc_rollup_refute] contents carrying [repr_refutation] (given at the
   [Sc_rollup_game_repr] level). *)
let mk_refute_contents ?(counter = 0) ~source ~opponent ~rollup ~repr_refutation
    () =
  Alpha_context.Manager_operation
    {
      source;
      fee = Alpha_context.Tez.zero;
      counter = Alpha_context.Manager_counter.Internal_for_tests.of_int counter;
      operation =
        Alpha_context.Sc_rollup_refute
          {
            rollup;
            opponent;
            refutation =
              (* Carry the refutation across the .mli seal between
                 [Sc_rollup_game_repr] and [Alpha_context.Sc_rollup.Game]
                 through the byte-identical wire encoding. *)
              Data_encoding.Binary.of_bytes_exn
                Alpha_context.Sc_rollup.Game.refutation_encoding
              @@ Data_encoding.Binary.to_bytes_exn
                   Sc_rollup_game_repr.refutation_encoding
                   repr_refutation;
          };
      gas_limit = Alpha_context.Gas.Arith.zero;
      storage_limit = Z.zero;
    }

let mk_packed contents_list : Alpha_context.packed_operation =
  let shell = Tezos_base.Operation.{branch = Block_hash.zero} in
  let protocol_data =
    Alpha_context.Operation_data {contents = contents_list; signature = None}
  in
  {shell; protocol_data}

let mk_refute_op ~source ~opponent ~rollup ~repr_refutation =
  mk_packed
    (Single (mk_refute_contents ~source ~opponent ~rollup ~repr_refutation ()))

(* A batch: one manager operation carrying two refutations from the same
   source. The [1M] rule allows a manager only one operation per block, so this
   is the only way two refutations from the same player share a block -- and
   the only way a [Start] and a [Move] on the game it opens can. *)
let mk_refute_batch_op ~source ~opponent ~rollup ~first ~second =
  mk_packed
    (Cons
       ( mk_refute_contents
           ~counter:0
           ~source
           ~opponent
           ~rollup
           ~repr_refutation:first
           (),
         Single
           (mk_refute_contents
              ~counter:1
              ~source
              ~opponent
              ~rollup
              ~repr_refutation:second
              ()) ))

(* The plugin state a node starts a block from, built on [ctxt_before] — the
   start-of-block context. *)
let init_plugin_state ctxt_before =
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
  return (Block_validation.init_block_validation_state validation_state)

(* Replay what the baker does with the plugin's verdict
   ([operation_selection.ml]): an operation the plugin refuses is left out of
   the block, and the plugin state is kept as it was. Returns, for each
   labelled operation and in order, whether it made it into the block. *)
let select_block_operations ~ctxt_before labelled_ops =
  let open Lwt_result_wrap_syntax in
  let* plugin_state = init_plugin_state ctxt_before in
  let*! selection =
    let open Lwt_syntax in
    let* _plugin_state, selection =
      List.fold_left_s
        (fun (plugin_state, selection) (label, op) ->
          let* res = Block_validation.check_block_operation plugin_state op in
          match res with
          | Ok plugin_state -> return (plugin_state, (label, true) :: selection)
          | Error _ -> return (plugin_state, (label, false) :: selection))
        (plugin_state, [])
        labelled_ops
    in
    return (List.rev selection)
  in
  return selection

let assert_in_block ~loc ~scenario selection label =
  let open Lwt_result_syntax in
  match List.assoc ~equal:String.equal label selection with
  | Some true -> return_unit
  | Some false ->
      Test.fail
        ~__LOC__:loc
        "%s: the %s operation was refused by the block validation plugin, so \
         the baker leaves it out of the block"
        scenario
        label
  | None -> Test.fail ~__LOC__:loc "%s: no verdict for %s" scenario label

let assert_not_in_block ~loc ~scenario selection label =
  let open Lwt_result_syntax in
  match List.assoc ~equal:String.equal label selection with
  | Some false -> return_unit
  | Some true ->
      Test.fail
        ~__LOC__:loc
        "%s: the %s operation was accepted by the block validation plugin, so \
         the baker includes it in the block"
        scenario
        label
  | None -> Test.fail ~__LOC__:loc "%s: no verdict for %s" scenario label

(* The well-formed dissection of the initial section that [game_move] accepts,
   as built by [test_single_valid_game_move]. *)
let valid_initial_dissection ctxt ~start_hash =
  let size =
    Constants_storage.sc_rollup_number_of_sections_in_dissection ctxt
  in
  let tick_per_state = 10_000 / size in
  Stdlib.List.init (size + 1) (fun i ->
      mk_dissection_chunk
      @@
      if i = 0 then (Some start_hash, tick_of_int_exn 0)
      else if i = size then (None, tick_of_int_exn 10000)
      else (Some (hash_int i), tick_of_int_exn (i * tick_per_state)))

(* A stub refutation [Move]: every operation built from it fails before the
   step is examined, so its contents do not matter. *)
let stub_move ~start_hash =
  G.Move
    {
      choice = Sc_rollup_tick_repr.initial;
      step = G.Dissection (init_dissection ~size:4 start_hash);
    }

(* A [Proof] step with no defect of its own: the PVM part is a genuine Arith
   PVM step and there is no input proof.

   What the plugin does with a [Proof] move depends on the section its [choice]
   names in the game's dissection, never on the proof's contents: the PVM
   replay is a no-op on the [Example_arith] rollup these tests originate, and
   the inbox-level check has nothing to compare without an input proof. *)
let build_proof_step () =
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
  let alpha_proof =
    Alpha_context.Sc_rollup.Proof.{pvm_step; input_proof = None}
  in
  (* Same .mli-seal crossing as in [mk_refute_op], in the other direction:
     [Alpha_context.Sc_rollup.Proof] and [Sc_rollup_proof_repr] are the same
     module underneath, so the wire encoding carries the value across. *)
  return
    (Data_encoding.Binary.of_bytes_exn Sc_rollup_proof_repr.encoding
    @@ Data_encoding.Binary.to_bytes_exn
         Alpha_context.Sc_rollup.Proof.encoding
         alpha_proof)

(* Shape 1 — a [Move] played out of turn ([Sc_rollup_wrong_turn]).

   The scenario: the game is running, it is the victim's turn,
   and the attacker's move can only fail. *)
let test_out_of_turn_move_does_not_exclude_the_honest_move () =
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
  let*@ ctxt_before =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  (* Per [Sc_rollup_game_repr.initial] the refuter is next to play, so the
     victim is the refuter and the attacker its opponent. *)
  let victim = refuter and attacker = defender in
  let index = G.Index.make refuter defender in
  let*@ _ctxt, game_opt = R.find_game ctxt_before rollup index in
  let* () =
    match game_opt with
    | Some game ->
        Assert.equal_bool
          ~loc:__LOC__
          (Sc_rollup_repr.Staker.equal
             (G.Index.staker index game.G.turn)
             victim)
          true
    | None -> Test.fail ~__LOC__ "out-of-turn: the game was not created"
  in
  let start_hash = hash_string "foo" in
  (* Ground truth 1: the victim's move is the one that applies. *)
  let*@ game_result, _ctxt =
    R.game_move
      ctxt_before
      rollup
      ~player:victim
      ~opponent:attacker
      ~choice:Sc_rollup_tick_repr.initial
      ~step:(G.Dissection (valid_initial_dissection ctxt_before ~start_hash))
  in
  let* () = Assert.is_none ~loc:__LOC__ ~pp:G.pp_game_result game_result in
  (* Ground truth 2: the attacker's move is inert — rejected on the turn
     check, before it can touch the game. *)
  let*!@ res =
    R.game_move
      ctxt_before
      rollup
      ~player:attacker
      ~opponent:victim
      ~choice:Sc_rollup_tick_repr.initial
      ~step:(G.Dissection (init_dissection ~size:4 start_hash))
  in
  let* () =
    assert_fails_with
      ~__LOC__
      (Lwt.return res)
      Sc_rollup_errors.Sc_rollup_wrong_turn
  in
  let honest_op =
    mk_refute_op
      ~source:victim
      ~opponent:attacker
      ~rollup
      ~repr_refutation:
        (G.Move
           {
             choice = Sc_rollup_tick_repr.initial;
             step =
               G.Dissection (valid_initial_dissection ctxt_before ~start_hash);
           })
  in
  let poison_op =
    mk_refute_op
      ~source:attacker
      ~opponent:victim
      ~rollup
      ~repr_refutation:(stub_move ~start_hash)
  in
  let scenario = "out-of-turn Move" in
  (* Control: alone, the honest move makes it into the block. *)
  let* selection =
    select_block_operations ~ctxt_before [("honest", honest_op)]
  in
  let* () = assert_in_block ~loc:__LOC__ ~scenario selection "honest" in
  (* The attacker's cheaper operation is selected first. It is refused -- it is
     not the move the game expects -- and the honest move still gets in. *)
  let* selection =
    select_block_operations
      ~ctxt_before
      [("poison", poison_op); ("honest", honest_op)]
  in
  let* () = assert_not_in_block ~loc:__LOC__ ~scenario selection "poison" in
  assert_in_block ~loc:__LOC__ ~scenario selection "honest"

(* Shape 2 — a [Start] for a game that is already started
   ([Sc_rollup_game_already_started]).

   Invisible to a check that only compares the turn of a [Move]: a [Start]
   carries no [choice] and no [step]. It is also the cheapest poison
   available — two commitment hashes, no dissection, no proof. *)
let test_start_on_started_game_does_not_exclude_the_honest_move () =
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
  let*@ ctxt_before =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  let victim = refuter and attacker = defender in
  let victim_commitment_hash = refuter_commitment_hash
  and attacker_commitment_hash = defender_commitment_hash in
  (* Ground truth 1: the operation is includable. [validate_sc_rollup_refute]
     gates a [Start] on [check_conflict_point], which is symmetric in the two
     roles and does not depend on the game existing — both stakers are still
     staked on their conflicting commitments for the whole game. *)
  let*!@ conflict_res =
    R.check_conflict_point
      ctxt_before
      rollup
      ~refuter:attacker
      ~refuter_commitment_hash:attacker_commitment_hash
      ~defender:victim
      ~defender_commitment_hash:victim_commitment_hash
  in
  let* () =
    match conflict_res with
    | Ok _ -> return_unit
    | Error trace ->
        Test.fail
          ~__LOC__
          "already-started: the validate-time gate rejects this Start, so the \
           shape is not includable and the premise of this test is stale: %a"
          pp_print_trace
          trace
  in
  (* Ground truth 2: it is inert — [start_game] checks [game_exists] first,
     before the staker and conflict checks. *)
  let*!@ res =
    R.start_game
      ctxt_before
      rollup
      ~player:(attacker, attacker_commitment_hash)
      ~opponent:(victim, victim_commitment_hash)
  in
  let* () =
    assert_fails_with
      ~__LOC__
      (Lwt.return res)
      Sc_rollup_errors.Sc_rollup_game_already_started
  in
  (* Ground truth 3: the victim's move is the one that applies. *)
  let start_hash = hash_string "foo" in
  let*@ game_result, _ctxt =
    R.game_move
      ctxt_before
      rollup
      ~player:victim
      ~opponent:attacker
      ~choice:Sc_rollup_tick_repr.initial
      ~step:(G.Dissection (valid_initial_dissection ctxt_before ~start_hash))
  in
  let* () = Assert.is_none ~loc:__LOC__ ~pp:G.pp_game_result game_result in
  let honest_op =
    mk_refute_op
      ~source:victim
      ~opponent:attacker
      ~rollup
      ~repr_refutation:
        (G.Move
           {
             choice = Sc_rollup_tick_repr.initial;
             step =
               G.Dissection (valid_initial_dissection ctxt_before ~start_hash);
           })
  in
  let poison_op =
    mk_refute_op
      ~source:attacker
      ~opponent:victim
      ~rollup
      ~repr_refutation:
        (G.Start
           {
             player_commitment_hash = attacker_commitment_hash;
             opponent_commitment_hash = victim_commitment_hash;
           })
  in
  let scenario = "Start on an already-started game" in
  let* selection =
    select_block_operations ~ctxt_before [("honest", honest_op)]
  in
  let* () = assert_in_block ~loc:__LOC__ ~scenario selection "honest" in
  let* selection =
    select_block_operations
      ~ctxt_before
      [("poison", poison_op); ("honest", honest_op)]
  in
  (* The rule reaches [Move]s only, so the [Start] is kept -- it changes
     nothing when it fails, and keeping it costs the honest move nothing. *)
  let* () = assert_in_block ~loc:__LOC__ ~scenario selection "poison" in
  assert_in_block ~loc:__LOC__ ~scenario selection "honest"

(* Shape 3 — a [Move] for a game that does not exist yet
   ([Sc_rollup_no_game]).

   Also invisible to a turn comparison: there is no game, hence no [turn] to
   read. What it censors is the victim's [Start], because [Index.make] sorts
   the pair — so the deadline is the challenge window and the prize is having
   the attacker's commitment cemented unchallenged. *)
let test_move_on_absent_game_does_not_exclude_the_honest_start () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt_before,
         rollup,
         refuter,
         defender,
         _staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  (* No game yet. The victim is the player about to open one; the attacker is
     the staker on the conflicting commitment. *)
  let victim = defender and attacker = refuter in
  let victim_commitment_hash = defender_commitment_hash
  and attacker_commitment_hash = refuter_commitment_hash in
  (* Ground truth 1: both operations land on the same gate key, because the
     game index is the sorted pair. *)
  let attacker_index = G.Index.make attacker victim in
  let victim_index = G.Index.make victim attacker in
  let* () =
    Assert.equal_bool
      ~loc:__LOC__
      (Sc_rollup_repr.Staker.equal
         attacker_index.G.Index.alice
         victim_index.G.Index.alice
      && Sc_rollup_repr.Staker.equal
           attacker_index.G.Index.bob
           victim_index.G.Index.bob)
      true
  in
  (* Ground truth 2: the attacker's move is inert. A [Move] gets no
     validate-time check at all ([validate_sc_rollup_refute] returns
     immediately), and fails at application on the absent game. *)
  let start_hash = hash_string "foo" in
  let*!@ res =
    R.game_move
      ctxt_before
      rollup
      ~player:attacker
      ~opponent:victim
      ~choice:Sc_rollup_tick_repr.initial
      ~step:(G.Dissection (init_dissection ~size:4 start_hash))
  in
  let* () =
    assert_fails_with
      ~__LOC__
      (Lwt.return res)
      Sc_rollup_errors.Sc_rollup_no_game
  in
  (* Ground truth 3: the victim's [Start] is the operation that applies. *)
  let*@ _ctxt =
    R.start_game
      ctxt_before
      rollup
      ~player:(victim, victim_commitment_hash)
      ~opponent:(attacker, attacker_commitment_hash)
  in
  let honest_op =
    mk_refute_op
      ~source:victim
      ~opponent:attacker
      ~rollup
      ~repr_refutation:
        (G.Start
           {
             player_commitment_hash = victim_commitment_hash;
             opponent_commitment_hash = attacker_commitment_hash;
           })
  in
  let poison_op =
    mk_refute_op
      ~source:attacker
      ~opponent:victim
      ~rollup
      ~repr_refutation:(stub_move ~start_hash)
  in
  let scenario = "Move on a game that does not exist yet" in
  let* selection =
    select_block_operations ~ctxt_before [("honest", honest_op)]
  in
  let* () = assert_in_block ~loc:__LOC__ ~scenario selection "honest" in
  let* selection =
    select_block_operations
      ~ctxt_before
      [("poison", poison_op); ("honest", honest_op)]
  in
  (* With no game there is no player to expect the move from, so it is
     refused; the honest [Start] is a [Start], and never is. *)
  let* () = assert_not_in_block ~loc:__LOC__ ~scenario selection "poison" in
  assert_in_block ~loc:__LOC__ ~scenario selection "honest"

(* A refutation naming a rollup that does not exist reads as a [Move] on a game
   that does not exist, so the rule refuses it. That is not a quirk of unknown
   rollups: [Sc_rollup_refutation_storage.find_game] answers [Ok None] rather
   than an error for a missing rollup, so this is the same verdict a [Move] on
   any absent game gets. It is the cheapest shape that reaches it -- anyone can
   sign one, for any address. *)
let test_move_on_an_unknown_rollup_is_refused () =
  let open Lwt_result_wrap_syntax in
  let* ctxt_before, rollup, refuter, defender, _staker3, _, _ =
    two_stakers_in_conflict ()
  in
  let unknown_rollup = Sc_rollup_repr.Address.zero in
  let* () =
    Assert.equal_bool
      ~loc:__LOC__
      (Sc_rollup_repr.Address.equal unknown_rollup rollup)
      false
  in
  let op =
    mk_refute_op
      ~source:refuter
      ~opponent:defender
      ~rollup:unknown_rollup
      ~repr_refutation:(stub_move ~start_hash:(hash_string "foo"))
  in
  let* selection =
    select_block_operations ~ctxt_before [("unknown rollup", op)]
  in
  assert_not_in_block
    ~loc:__LOC__
    ~scenario:"Move naming an unknown rollup"
    selection
    "unknown rollup"

(* ------------------------------------------------------------------------ *)
(* What must not be refused, and the checks that must stay sound. *)

(* Both players may legitimately open the game, so both [Start]s belong in the
   block.

   This is the only window in which two operations on the same game genuinely
   compete: once the game is running, only the player whose turn it is has a
   move to make. And it is the window in which an operation doomed at
   application does the most damage, because the loser of the race is the
   victim: sustained to the end of the challenge window, the attacker's
   commitment is cemented unchallenged.

   Gas is why refusing one of them could not be made safe by predicting which
   one fails: [start_game] pays for every carbonated read it makes, and whether
   an operation's gas covers them is settled by running it, not by inspecting
   it. *)
let test_competing_starts_are_both_included () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt_before,
         rollup,
         refuter,
         defender,
         _staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  (* No game yet, and the two stakers are on conflicting commitments: either
     may open the game against the other. *)
  let attacker = refuter and victim = defender in
  let attacker_commitment_hash = refuter_commitment_hash
  and victim_commitment_hash = defender_commitment_hash in
  (* Ground truth 1: the first [Start] to apply opens the game, and the other
     then fails with [Sc_rollup_game_already_started]. *)
  let*@ ctxt_after_attacker =
    R.start_game
      ctxt_before
      rollup
      ~player:(attacker, attacker_commitment_hash)
      ~opponent:(victim, victim_commitment_hash)
  in
  let*@ _ctxt, game_opt =
    R.find_game ctxt_after_attacker rollup (G.Index.make attacker victim)
  in
  let* () =
    match game_opt with
    | Some _ -> return_unit
    | None ->
        Test.fail ~__LOC__ "competing Starts: the first Start opened no game"
  in
  let*!@ res =
    R.start_game
      ctxt_after_attacker
      rollup
      ~player:(victim, victim_commitment_hash)
      ~opponent:(attacker, attacker_commitment_hash)
  in
  let* () =
    assert_fails_with
      ~__LOC__
      (Lwt.return res)
      Sc_rollup_errors.Sc_rollup_game_already_started
  in
  (* Ground truth 2: the attacker's [Start] may also fail at application on the
     gas it declared, which is a property of the operation the plugin has no
     way to decide -- the same operation on the same state applies or fails
     depending on a budget only execution consumes. *)
  let starved_ctxt =
    Raw_context.set_gas_limit
      ctxt_before
      (Gas_limit_repr.Arith.integral_of_int_exn 100)
  in
  let*!@ res =
    R.start_game
      starved_ctxt
      rollup
      ~player:(attacker, attacker_commitment_hash)
      ~opponent:(victim, victim_commitment_hash)
  in
  let* () =
    Assert.proto_error_with_info
      ~loc:__LOC__
      res
      "Gas quota exceeded for the operation"
  in
  (* Ground truth 3: the victim's [Start] is then the one that opens the
     game. *)
  let*@ _ctxt =
    R.start_game
      ctxt_before
      rollup
      ~player:(victim, victim_commitment_hash)
      ~opponent:(attacker, attacker_commitment_hash)
  in
  let mk_start ~source ~source_commitment_hash ~opponent
      ~opponent_commitment_hash =
    mk_refute_op
      ~source
      ~opponent
      ~rollup
      ~repr_refutation:
        (G.Start
           {
             player_commitment_hash = source_commitment_hash;
             opponent_commitment_hash;
           })
  in
  let attacker_start =
    mk_start
      ~source:attacker
      ~source_commitment_hash:attacker_commitment_hash
      ~opponent:victim
      ~opponent_commitment_hash:victim_commitment_hash
  in
  let victim_start =
    mk_start
      ~source:victim
      ~source_commitment_hash:victim_commitment_hash
      ~opponent:attacker
      ~opponent_commitment_hash:attacker_commitment_hash
  in
  let scenario = "both players racing to Start" in
  let* selection =
    select_block_operations
      ~ctxt_before
      [("attacker Start", attacker_start); ("victim Start", victim_start)]
  in
  let* () = assert_in_block ~loc:__LOC__ ~scenario selection "attacker Start" in
  assert_in_block ~loc:__LOC__ ~scenario selection "victim Start"

(* The bypass the checks exist for: a [Proof] move answering a dissection made
   earlier in the same block.

   The [Proof] checks read the dissection as it stood at the start of the
   block. A dissection move refreshes it, so a [Proof] on a section of the NEW
   dissection is looked up in a section list that does not contain it --
   [find_section_around_choice] finds no section, and both the multi-tick and
   the missing-start-state checks fall through. Applied, it would anchor the
   game on a section no single-tick proof can span.

   The rule closes it without looking at what came before: the dissection move
   is from the player the game expects, the answering [Proof] is not. *)
let test_opponent_proof_on_a_refreshed_dissection_is_refused () =
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
  let*@ ctxt_before =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  (* The refuter is next to play, so the block opens with its dissection move
     and the defender answers. *)
  let start_hash = hash_string "foo" in
  let op_dissection =
    mk_refute_op
      ~source:refuter
      ~opponent:defender
      ~rollup
      ~repr_refutation:
        (G.Move
           {
             choice = Sc_rollup_tick_repr.initial;
             step =
               G.Dissection (valid_initial_dissection ctxt_before ~start_hash);
           })
  in
  let* proof = build_proof_step () in
  (* The start-of-block dissection is [parent@0; child@10000; None@10001], so
     tick 5000 is a section boundary of the refreshed dissection only. *)
  let op_proof =
    mk_refute_op
      ~source:defender
      ~opponent:refuter
      ~rollup
      ~repr_refutation:
        (G.Move {choice = tick_of_int_exn 5000; step = G.Proof proof})
  in
  (* Ground truth: tick 5000 is not a chunk of the start-of-block dissection,
     so [find_section_around_choice] finds no section there and the checks have
     nothing to look at. *)
  let*@ _ctxt, game_opt =
    R.find_game ctxt_before rollup (G.Index.make refuter defender)
  in
  let* () =
    match game_opt with
    | Some {G.game_state = G.Dissecting {dissection; _}; _} ->
        Assert.equal_bool
          ~loc:__LOC__
          (List.exists
             (fun (c : D.t) ->
               Sc_rollup_tick_repr.equal c.D.tick (tick_of_int_exn 5000))
             dissection)
          false
    | _ -> Test.fail ~__LOC__ "refreshed dissection: the game is not Dissecting"
  in
  let scenario = "Proof answering a dissection made in the same block" in
  let* selection =
    select_block_operations
      ~ctxt_before
      [("dissection", op_dissection); ("proof", op_proof)]
  in
  let* () = assert_in_block ~loc:__LOC__ ~scenario selection "dissection" in
  assert_not_in_block ~loc:__LOC__ ~scenario selection "proof"

(* A [Start] and a [Move] on the game it opens, batched into one operation.

   The [1M] rule gives a manager one operation per block, so a batch is the
   only way a player can both open a game and play in it in the same block --
   and [start_game] puts its own sender on turn, so that [Move] is on turn when
   it applies. The start-of-block context has no game to check it against, so
   the rule refuses a [Move] whose game does not exist yet. *)
let test_move_batched_after_a_start_is_refused () =
  let open Lwt_result_wrap_syntax in
  let* ( ctxt_before,
         rollup,
         refuter,
         defender,
         _staker3,
         refuter_commitment_hash,
         defender_commitment_hash ) =
    two_stakers_in_conflict ()
  in
  let* proof = build_proof_step () in
  (* Ground truth: applied in this order, the pair moves the game. The [Start]
     opens it with the refuter on turn, and the [Proof] -- played on the whole
     10000-tick section, which no single PVM step can span -- is accepted as a
     move and anchors the game on a [Final_move] neither player can answer. *)
  let*@ ctxt_after_start =
    R.start_game
      ctxt_before
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  let*@ game_result, ctxt_after_proof =
    R.game_move
      ctxt_after_start
      rollup
      ~player:refuter
      ~opponent:defender
      ~choice:Sc_rollup_tick_repr.initial
      ~step:(G.Proof proof)
  in
  let* () = Assert.is_none ~loc:__LOC__ ~pp:G.pp_game_result game_result in
  let*@ _ctxt, game_opt =
    R.find_game ctxt_after_proof rollup (G.Index.make refuter defender)
  in
  let* () =
    match game_opt with
    | Some
        {
          G.game_state = G.Final_move {agreed_start_chunk; refuted_stop_chunk};
          _;
        } ->
        Assert.equal_bool
          ~loc:__LOC__
          Z.(
            gt
              (Sc_rollup_tick_repr.distance
                 agreed_start_chunk.D.tick
                 refuted_stop_chunk.D.tick)
              one)
          true
    | _ ->
        Test.fail
          ~__LOC__
          "batched Start+Proof: the Proof did not anchor a Final_move, so this \
           test no longer describes what it must prevent"
  in
  let batch =
    mk_refute_batch_op
      ~source:refuter
      ~opponent:defender
      ~rollup
      ~first:
        (G.Start
           {
             player_commitment_hash = refuter_commitment_hash;
             opponent_commitment_hash = defender_commitment_hash;
           })
      ~second:
        (G.Move {choice = Sc_rollup_tick_repr.initial; step = G.Proof proof})
  in
  let* selection = select_block_operations ~ctxt_before [("batch", batch)] in
  assert_not_in_block
    ~loc:__LOC__
    ~scenario:"Start and Move batched in one operation"
    selection
    "batch"

(* The move the game expects is still checked against the start-of-block state,
   and kept or refused on its own merits. If the rule replaced the check, the
   second [Proof] below would be kept although the section it plays on is 10000
   ticks wide. *)
let test_proof_is_checked_against_the_block_state () =
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
  let*@ ctxt_before =
    R.start_game
      ctxt
      rollup
      ~player:(refuter, refuter_commitment_hash)
      ~opponent:(defender, defender_commitment_hash)
  in
  (* The refuter is next to play, so its moves are the ones the rule lets
     through. *)
  let* proof = build_proof_step () in
  let mk_proof_op choice =
    mk_refute_op
      ~source:refuter
      ~opponent:defender
      ~rollup
      ~repr_refutation:(G.Move {choice; step = G.Proof proof})
  in
  (* In the start-of-block dissection [parent@0; child@10000; None@10001],
     tick 10000 opens the single-tick section the check accepts and tick 0 the
     10000-tick one it refuses. *)
  let scenario = "Proof from the expected player" in
  let* selection =
    select_block_operations
      ~ctxt_before
      [("proof", mk_proof_op (tick_of_int_exn 10000))]
  in
  let* () = assert_in_block ~loc:__LOC__ ~scenario selection "proof" in
  let* selection =
    select_block_operations
      ~ctxt_before
      [("proof", mk_proof_op Sc_rollup_tick_repr.initial)]
  in
  assert_not_in_block ~loc:__LOC__ ~scenario selection "proof"

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
      "a Move played out of turn is refused and does not keep the honest move \
       out of the block."
      `Quick
      test_out_of_turn_move_does_not_exclude_the_honest_move;
    Tztest.tztest
      "a Start on an already-started game is kept and does not keep the honest \
       move out of the block."
      `Quick
      test_start_on_started_game_does_not_exclude_the_honest_move;
    Tztest.tztest
      "a Move on a game that does not exist yet is refused and does not keep \
       the honest Start out of the block."
      `Quick
      test_move_on_absent_game_does_not_exclude_the_honest_start;
    Tztest.tztest
      "a Move naming an unknown rollup is refused."
      `Quick
      test_move_on_an_unknown_rollup_is_refused;
    Tztest.tztest
      "Two players racing to Start the same game are both included in the \
       block."
      `Quick
      test_competing_starts_are_both_included;
    Tztest.tztest
      "A Proof answering a dissection made in the same block is refused."
      `Quick
      test_opponent_proof_on_a_refreshed_dissection_is_refused;
    Tztest.tztest
      "A Start and a Move batched into one operation are refused."
      `Quick
      test_move_batched_after_a_start_is_refused;
    Tztest.tztest
      "A Proof is still checked against the start-of-block state."
      `Quick
      test_proof_is_checked_against_the_block_state;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("sc rollup game", tests)]
  |> Lwt_main.run
