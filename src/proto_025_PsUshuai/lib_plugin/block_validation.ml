(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
module Sr = Sc_rollup
module Game = Sr.Game
module Proof = Sr.Proof
module Inbox = Sr.Inbox
module Payloads = Sr.Inbox_merkelized_payload_hashes

(* [context] is the context at the START of the block: it is not updated as
   operations are applied. The refutation checks below therefore describe the
   state a move will actually meet only as long as no earlier operation in the
   block has moved that game.

   That is what [check_refute_move_expected] buys, with no state to thread:
   only the player the game expects at the start of the block may play a [Move]
   on it. Every move [Sc_rollup_game_repr.play] accepts flips the turn, so once
   one has applied, every other [Move] on that game fails the protocol's own
   [Sc_rollup_wrong_turn] check before reaching [play]. At most one [Move] per
   game per block can therefore change anything, and it is the one this context
   describes.

   [Start] is deliberately left alone: it has no turn to be out of, no check
   here reads the state for it, and before the game exists both players may
   legitimately open it — a race the plugin must not arbitrate. *)
type block_validation_state = {context : context}

let init_block_validation_state validation_state : block_validation_state =
  let context = Validate.get_initial_ctxt validation_state in
  {context}

let shell_fail err : 'a Environment.Error_monad.shell_tzresult Lwt.t =
  Lwt.return_error [Environment.wrap_tzerror err]

type Environment.Error_monad.error +=
  | Sc_rollup_proof_on_multi_tick_section_during_dissecting of Z.t
  | Sc_rollup_proof_on_missing_start_state_during_dissecting of Z.t
  | Sc_rollup_refute_move_out_of_turn of {
      rollup : Sr.Address.t;
      stakers : Game.Index.t;
      source : Sr.Staker.t;
      expected : Sr.Staker.t;
    }
  | Sc_rollup_refute_move_on_absent_game of {
      rollup : Sr.Address.t;
      stakers : Game.Index.t;
    }
  | Sc_rollup_proof_on_game_started_in_same_operation of {
      rollup : Sr.Address.t;
      stakers : Game.Index.t;
    }
  | Invalid_double_baking_evidence of {level : Int32.t}
  | Sc_rollup_inbox_proof_claimed_level_mismatch of {
      claimed_level : Raw_level.t;
      proven_level : Raw_level.t;
    }
  | Sc_rollup_inbox_proof_missing_index_proof of {head : Payloads.Hash.t}
  | Sc_rollup_inbox_proof_relabelled_indices of {head : Payloads.Hash.t}
  | Sc_rollup_inbox_proof_unreadable_cells of {head : Payloads.Hash.t}

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Permanent
    ~id:
      "block_validation_plugin.sc_rollup_proof_on_multi_tick_section_during_dissecting"
    ~title:"Proof submitted on a multi-tick section during dissecting"
    ~description:
      "A refutation game player submitted a Proof move during the Dissecting \
       phase on a section whose tick distance is greater than one."
    ~pp:(fun ppf distance ->
      Format.fprintf
        ppf
        "Proof submitted on a multi-tick section (distance %s) during \
         dissecting"
        (Z.to_string distance))
    Data_encoding.(obj1 (req "distance" (conv Z.to_string Z.of_string string)))
    (function
      | Sc_rollup_proof_on_multi_tick_section_during_dissecting distance ->
          Some distance
      | _ -> None)
    (fun distance ->
      Sc_rollup_proof_on_multi_tick_section_during_dissecting distance) ;
  register_error_kind
    `Permanent
    ~id:
      "block_validation_plugin.sc_rollup_proof_on_missing_start_state_during_dissecting"
    ~title:"Proof submitted on a section with no start state during dissecting"
    ~description:
      "A refutation game player submitted a Proof move during the Dissecting \
       phase on a section whose agreed start state is absent."
    ~pp:(fun ppf tick ->
      Format.fprintf
        ppf
        "Proof submitted during dissecting on a section starting at tick %s \
         whose agreed start state is absent"
        (Z.to_string tick))
    Data_encoding.(obj1 (req "tick" (conv Z.to_string Z.of_string string)))
    (function
      | Sc_rollup_proof_on_missing_start_state_during_dissecting tick ->
          Some tick
      | _ -> None)
    (fun tick -> Sc_rollup_proof_on_missing_start_state_during_dissecting tick) ;
  register_error_kind
    `Temporary
    ~id:"block_validation_plugin.sc_rollup_refute_move_out_of_turn"
    ~title:"Refutation Move from a player the game does not expect"
    ~description:
      "A block contains a smart rollup refutation Move from a player other \
       than the one whose turn it was at the start of the block. The player \
       may inject it again in a later block."
    ~pp:(fun ppf (rollup, stakers, source, expected) ->
      Format.fprintf
        ppf
        "Refutation Move from %a on the game between %a and %a on rollup %a, \
         which expects a move from %a"
        Sr.Staker.pp
        source
        Sr.Staker.pp
        stakers.Game.Index.alice
        Sr.Staker.pp
        stakers.Game.Index.bob
        Sr.Address.pp
        rollup
        Sr.Staker.pp
        expected)
    Data_encoding.(
      obj4
        (req "rollup" Sr.Address.encoding)
        (req "stakers" Game.Index.encoding)
        (req "source" Sr.Staker.encoding)
        (req "expected" Sr.Staker.encoding))
    (function
      | Sc_rollup_refute_move_out_of_turn {rollup; stakers; source; expected} ->
          Some (rollup, stakers, source, expected)
      | _ -> None)
    (fun (rollup, stakers, source, expected) ->
      Sc_rollup_refute_move_out_of_turn {rollup; stakers; source; expected}) ;
  register_error_kind
    `Temporary
    ~id:"block_validation_plugin.sc_rollup_refute_move_on_absent_game"
    ~title:"Refutation Move on a game that does not exist yet"
    ~description:
      "A block contains a smart rollup refutation Move on a refutation game \
       that did not exist at the start of the block. The player may inject it \
       again in a later block."
    ~pp:(fun ppf (rollup, stakers) ->
      Format.fprintf
        ppf
        "Refutation Move on the game between %a and %a on rollup %a, which \
         does not exist at the start of the block"
        Sr.Staker.pp
        stakers.Game.Index.alice
        Sr.Staker.pp
        stakers.Game.Index.bob
        Sr.Address.pp
        rollup)
    Data_encoding.(
      obj2
        (req "rollup" Sr.Address.encoding)
        (req "stakers" Game.Index.encoding))
    (function
      | Sc_rollup_refute_move_on_absent_game {rollup; stakers} ->
          Some (rollup, stakers)
      | _ -> None)
    (fun (rollup, stakers) ->
      Sc_rollup_refute_move_on_absent_game {rollup; stakers}) ;
  register_error_kind
    `Permanent
    ~id:
      "block_validation_plugin.sc_rollup_proof_on_game_started_in_same_operation"
    ~title:"Refutation Proof move on a game opened by the same operation"
    ~description:
      "A block contains an operation that both opens a smart rollup refutation \
       game and plays a Proof move in it. That move is applied to a game the \
       block did not start from, so it cannot be checked against the state the \
       block started from, and it is rejected."
    ~pp:(fun ppf (rollup, stakers) ->
      Format.fprintf
        ppf
        "Refutation Proof move on the game between %a and %a on rollup %a, \
         opened by the same operation"
        Sr.Staker.pp
        stakers.Game.Index.alice
        Sr.Staker.pp
        stakers.Game.Index.bob
        Sr.Address.pp
        rollup)
    Data_encoding.(
      obj2
        (req "rollup" Sr.Address.encoding)
        (req "stakers" Game.Index.encoding))
    (function
      | Sc_rollup_proof_on_game_started_in_same_operation {rollup; stakers} ->
          Some (rollup, stakers)
      | _ -> None)
    (fun (rollup, stakers) ->
      Sc_rollup_proof_on_game_started_in_same_operation {rollup; stakers}) ;
  register_error_kind
    `Permanent
    ~id:"block_validation_plugin.invalid_double_baking_evidence"
    ~title:"invalid double baking evidence"
    ~description:"Invalid double baking evidence."
    ~pp:(fun ppf level ->
      Format.fprintf ppf "Invalid double baking evidence at level %ld" level)
    Data_encoding.(obj1 (req "level" int32))
    (function
      | Invalid_double_baking_evidence {level} -> Some level | _ -> None)
    (fun level -> Invalid_double_baking_evidence {level}) ;
  register_error_kind
    `Permanent
    ~id:"block_validation_plugin.sc_rollup_inbox_proof_claimed_level_mismatch"
    ~title:"Inbox proof claims a level different from the one it proves"
    ~description:
      "A refutation Proof move carries an inbox proof whose claimed level does \
       not match the inbox level actually proven by its inclusion proof. Such \
       a proof lets a message from one inbox level be passed off as belonging \
       to another level, so the operation is rejected."
    ~pp:(fun ppf (claimed_level, proven_level) ->
      Format.fprintf
        ppf
        "Inbox proof claims level %a but its inclusion proof proves level %a"
        Raw_level.pp
        claimed_level
        Raw_level.pp
        proven_level)
    Data_encoding.(
      obj2
        (req "claimed_level" Raw_level.encoding)
        (req "proven_level" Raw_level.encoding))
    (function
      | Sc_rollup_inbox_proof_claimed_level_mismatch
          {claimed_level; proven_level} ->
          Some (claimed_level, proven_level)
      | _ -> None)
    (fun (claimed_level, proven_level) ->
      Sc_rollup_inbox_proof_claimed_level_mismatch {claimed_level; proven_level}) ;
  register_error_kind
    `Permanent
    ~id:"block_validation_plugin.sc_rollup_inbox_proof_missing_index_proof"
    ~title:"Inbox proof comes without the index proof of its level"
    ~description:
      "A refutation Proof move carries an inbox proof without the index proof \
       of the head cell of its level. The cells of the shared inbox are \
       authenticated by a hash that leaves their index out, and the protocol \
       does not record how many messages a level holds, so that index proof is \
       what establishes the position the proof claims. It has to be published \
       by a Sc_rollup_add_messages operation of the same batch."
    ~pp:(fun ppf head ->
      Format.fprintf
        ppf
        "Inbox proof comes without the index proof of the head cell %a"
        Payloads.Hash.pp
        head)
    Data_encoding.(obj1 (req "head" Payloads.Hash.encoding))
    (function
      | Sc_rollup_inbox_proof_missing_index_proof {head} -> Some head
      | _ -> None)
    (fun head -> Sc_rollup_inbox_proof_missing_index_proof {head}) ;
  register_error_kind
    `Permanent
    ~id:"block_validation_plugin.sc_rollup_inbox_proof_relabelled_indices"
    ~title:"Inbox proof discloses indices its skip list does not prescribe"
    ~description:
      "A refutation Proof move carries an inbox proof whose cells declare \
       indices that the shape of their skip list contradicts. Such a proof \
       presents a message at a position it does not occupy, so the operation \
       is rejected."
    ~pp:(fun ppf head ->
      Format.fprintf
        ppf
        "Inbox proof discloses relabelled payload hash indices below the head \
         cell %a"
        Payloads.Hash.pp
        head)
    Data_encoding.(obj1 (req "head" Payloads.Hash.encoding))
    (function
      | Sc_rollup_inbox_proof_relabelled_indices {head} -> Some head | _ -> None)
    (fun head -> Sc_rollup_inbox_proof_relabelled_indices {head}) ;
  register_error_kind
    `Permanent
    ~id:"block_validation_plugin.sc_rollup_inbox_proof_unreadable_cells"
    ~title:"Inbox proof discloses cells this node cannot read back"
    ~description:
      "A refutation Proof move carries an inbox proof whose cells could not be \
       read back from their own encoding, which leaves their indices \
       unchecked. The operation is rejected rather than let through \
       unverified."
    ~pp:(fun ppf head ->
      Format.fprintf
        ppf
        "Inbox proof discloses payload hash cells below the head cell %a that \
         this node cannot read back"
        Payloads.Hash.pp
        head)
    Data_encoding.(obj1 (req "head" Payloads.Hash.encoding))
    (function
      | Sc_rollup_inbox_proof_unreadable_cells {head} -> Some head | _ -> None)
    (fun head -> Sc_rollup_inbox_proof_unreadable_cells {head})

(* The refutation games a [Start] in this operation opens.

   A game can be destroyed and reopened under the same key inside one block --
   [Sc_rollup_timeout] removes it, a [Start] creates a new one -- and the two
   are then different games with the same stakers. [start_game] puts its own
   sender on turn, so only that sender can play in the game it just opened, and
   [1M] gives a manager one operation per block: the [Start] and the move that
   exploits it are necessarily contents of the same operation. Looking no
   further than this operation is therefore enough. *)
let games_started_in_operation contents =
  List.filter_map
    (function
      | Contents
          (Manager_operation
             {
               source;
               operation = Sc_rollup_refute {rollup; opponent; refutation};
               _;
             }) -> (
          match refutation with
          | Game.Start _ -> Some (rollup, Game.Index.make source opponent)
          | Game.Move _ -> None)
      | _ -> None)
    contents

let game_key_equal (rollup1, stakers1) (rollup2, stakers2) =
  Sr.Address.equal rollup1 rollup2
  && Sr.Staker.equal stakers1.Game.Index.alice stakers2.Game.Index.alice
  && Sr.Staker.equal stakers1.Game.Index.bob stakers2.Game.Index.bob

(* [check_refute_move_expected] accepts a refutation [Move] only from the player
   the game expects at the start of the block.

   Refusing the others costs them nothing they cannot recover: a move played out
   of turn can only fail at application ([Sc_rollup_wrong_turn]), and its sender
   may inject it again once the turn is theirs — one block out of the
   [timeout_period_in_blocks] their clock is measured in.

   What it buys is that the start-of-block [context] describes the state the
   accepted move will meet. Every move [play] accepts flips the turn, so the
   first one to apply makes every later [Move] on that game fail before [play];
   only one of them can change anything, and it is the one checked here. *)
let check_refute_move_expected context rollup stakers source :
    unit Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*! find_result = Sr.Refutation_storage.find_game context rollup stakers in
  match find_result with
  | Error _ ->
      (* The read itself failed. That is not a missing game or a missing
         rollup, both of which answer [Ok None]. [game_move] makes the same
         read, so this move cannot change the game either, and nothing it does
         can mislead a check on a later operation. Leave it in the block. *)
      return_unit
  | Ok (_ctxt, None) ->
      (* No game, hence no player to expect this move from. An earlier
         operation in the block may still have opened one — [start_game] puts
         its own sender on turn, and a batch can hold the [Start] and the
         [Move] — in which case the move applies against a game this context
         knows nothing about. *)
      shell_fail (Sc_rollup_refute_move_on_absent_game {rollup; stakers})
  | Ok (_ctxt, Some game) ->
      let expected =
        match game.Game.turn with
        | Game.Alice -> stakers.Game.Index.alice
        | Game.Bob -> stakers.Game.Index.bob
      in
      if Sr.Staker.equal expected source then return_unit
      else
        shell_fail
          (Sc_rollup_refute_move_out_of_turn {rollup; stakers; source; expected})

let find_section_around_choice dissection choice =
  let rec traverse = function
    | curr :: (next :: _ as rest) ->
        if Sr.Tick.equal curr.Sr.Dissection_chunk.tick choice then
          Ok (curr, next)
        else traverse rest
    | _ -> Error `Choice_not_found
  in
  traverse dissection

(* Reject an inbox proof whose claimed [level] disagrees with the level
   actually proven by its inclusion proof, closing the inbox level-confusion
   soundness bug: [Sc_rollup_inbox_repr.verify_proof] never binds the two, so a
   dishonest player could otherwise feed the PVM a real message at the wrong
   level. The proven level is read from the inclusion proof's target (last)
   cell. *)
let check_inbox_proof_level (proof : Proof.serialized Proof.t) =
  let open Environment.Error_monad.Lwt_result_syntax in
  match proof.Proof.input_proof with
  | Some (Proof.Inbox_proof {level = claimed_level; proof = serialized; _}) -> (
      match Inbox.of_serialized_proof serialized with
      | None ->
          (* Malformed proof: leave rejection to the protocol. *)
          return_unit
      | Some inbox_proof -> (
          let inclusion_proof, _payloads_proof =
            Inbox.Internal_for_tests.expose_proof inbox_proof
          in
          match List.last_opt inclusion_proof with
          | None ->
              (* Empty inclusion proof: leave rejection to the protocol. *)
              return_unit
          | Some target_cell ->
              let proven_level =
                (* [level_proof_of_history_proof] is re-exported through
                   [Alpha_context] (unlike [get_level_of_history_proof], which
                   stays behind the repr seal), so we read the level directly
                   from the target cell without an encoding round trip. It still
                   leaks [Raw_level_repr.t] rather than the abstract
                   [Alpha_context.Raw_level.t], so we bridge through [int32]. *)
                Raw_level.of_int32_exn @@ Raw_level_repr.to_int32
                @@ (Inbox.Internal_for_tests.level_proof_of_history_proof
                      target_cell)
                     .level
              in
              if Raw_level.equal proven_level claimed_level then return_unit
              else
                tzfail
                  (Sc_rollup_inbox_proof_claimed_level_mismatch
                     {claimed_level; proven_level})))
  | _ ->
      (* Not an inbox proof (reveal / first inbox message / no input proof). *)
      return_unit

(* A payload hash cell is hashed without its index, so
   [Sc_rollup_inbox_repr.verify_proof] reads indices that nothing binds: a
   proof showing a message at a place it does not hold still verifies.

   The path is checked here against the shape a skip list must have, which
   needs the index it starts at. Nothing records how many messages a level
   holds, so that index comes from an index proof the batch publishes;
   [index_proofs] holds those, keyed by the cell each one is about.

   The level path needs no index check. [verify_inclusion_proof] follows
   authenticated back pointers only, so the path ends at a true ancestor of the
   game's snapshot; [check_inbox_proof_level] pins that cell's level, which its
   hash covers; and the protocol appends one cell per level. Those three fix
   which cell the path ends at, whatever indices it declares.

   The check reads nothing but the operation, so the mempool runs it too. *)
let check_payload_indices ~index_proofs (proof : Proof.serialized Proof.t) =
  let open Environment.Error_monad.Result_syntax in
  match proof.Proof.input_proof with
  | Some (Proof.Inbox_proof {proof = serialized; _}) -> (
      match Inbox.of_serialized_proof serialized with
      | None ->
          (* Malformed proof: leave rejection to the protocol. *)
          return_unit
      | Some inbox_proof -> (
          let _inclusion_proof, payloads_proof =
            Inbox.Internal_for_tests.expose_proof inbox_proof
          in
          match
            (payloads_proof.Inbox.Internal_for_tests.proof :> Payloads.t list)
          with
          | [] ->
              (* Empty proof: leave rejection to the protocol. *)
              return_unit
          | head :: _ as payloads_cells -> (
              let head_hash = Payloads.hash head in
              let* index_proof =
                match
                  List.assoc ~equal:Payloads.Hash.equal head_hash index_proofs
                with
                | Some index_proof -> return index_proof
                | None ->
                    tzfail
                      (Sc_rollup_inbox_proof_missing_index_proof
                         {head = head_hash})
              in
              match
                Inbox_index_proof.verify_payloads_indices
                  ~inclusion:payloads_cells
                  ~index_proof
              with
              | Inbox_index_proof.Valid -> return_unit
              | Inconsistent_indices ->
                  tzfail
                    (Sc_rollup_inbox_proof_relabelled_indices {head = head_hash})
              | Unconvertible_cells ->
                  tzfail
                    (Sc_rollup_inbox_proof_unreadable_cells {head = head_hash}))
          ))
  | _ ->
      (* Not an inbox proof (reveal / first inbox message / no input proof). *)
      return_unit

(* The index proofs a batch publishes, keyed by the cell each one is about. A
   move may come before or after the operation carrying the index proof it
   needs, as long as both sit in the same batch. *)
let index_proofs_of_batch
    ({protocol_data = Operation_data {contents; _}; _} : packed_operation) =
  List.filter_map
    (function
      | [] -> None
      | head :: _ as index_proof -> Some (Payloads.hash head, index_proof))
    (List.concat_map
       (function
         | Contents
             (Manager_operation
                {operation = Sc_rollup_add_messages {messages}; _}) ->
             Inbox_index_proof.of_messages messages
         | _ -> [])
       (Operation.to_list (Contents_list contents)))

(* Whichever refutation Proof move of [op] carries an inbox proof its own batch
   does not back, or backs with indices the skip list contradicts. The mempool
   runs this so a forged move stops at the first node that reads it, instead of
   travelling to a baker that would drop it at forging. *)
let find_relabelled_inbox_proof
    ({protocol_data = Operation_data {contents; _}; _} as op : packed_operation)
    =
  (* Only a batch with a refutation Proof move needs its index proofs. *)
  let index_proofs = lazy (index_proofs_of_batch op) in
  List.find_map
    (function
      | Contents
          (Manager_operation
             {
               operation =
                 Sc_rollup_refute
                   {refutation = Game.Move {step = Game.Proof proof; _}; _};
               _;
             }) -> (
          match
            check_payload_indices ~index_proofs:(Lazy.force index_proofs) proof
          with
          | Ok () -> None
          | Error trace -> Some trace)
      | _ -> None)
    (Operation.to_list (Contents_list contents))

let protect_use_wasm_pvm ?config context rollup k =
  let open Environment.Error_monad.Lwt_result_syntax in
  let* _ctxt, kind = Sr.kind context rollup in
  match kind with
  | Wasm_2_0_0 ->
      Lwt.catch
        (fun () ->
          let* config =
            match config with
            | None ->
                let+ signals = Sr.Internal_for_tests.signals context in
                List.map
                  (fun (name, level) -> (name, Raw_level.to_int32 level))
                  signals
            | Some config -> return config
          in
          k (Sr.Wasm_2_0_0PVM.protocol_implementation ~config))
        (function
          | (Stack_overflow | Out_of_memory | Lwt.Canceled) as exn ->
              Lwt.reraise exn
          | _exn -> tzfail (Proof.Sc_rollup_proof_check "Invalid proof"))
  | _ -> return_unit

(* Replay, in the plugin, the PVM verification the protocol will perform when
   applying the operation, and reject the operation if it raises an exception
   (typically from the Irmin proof-replay layer) instead of returning an error:
   such an exception escapes [lib_protocol] and aborts block application. *)
let check_proof context rollup game (proof : Proof.serialized Proof.t) =
  let open Environment.Error_monad.Lwt_result_syntax in
  protect_use_wasm_pvm context rollup @@ fun (module PVM) ->
  match
    Data_encoding.Binary.of_string_opt
      PVM.proof_encoding
      (proof.Proof.pvm_step :> string)
  with
  | None ->
      tzfail (Proof.Sc_rollup_proof_check "Cannot decode refutation PVM proof")
  | Some pvm_step ->
      let reveal_activation_level =
        Constants.sc_rollup_reveal_activation_level context
      in
      let is_reveal_enabled =
        Sr.is_reveal_enabled_predicate reveal_activation_level
      in
      let dal_activation_level =
        if (Constants.parametric context).dal.feature_enable then
          Some reveal_activation_level.dal_parameters
        else None
      in
      let dal_attested_slots_validity_lag =
        reveal_activation_level.dal_attested_slots_validity_lag
      in
      let* _ctxt, genesis_info = Sr.genesis_info context rollup in
      let metadata =
        Sr.Metadata.{address = rollup; origination_level = genesis_info.level}
      in
      (* As long as we don't raise an exception, we are happy.
         Proof correctness is asserted by the protocol. *)
      let*! (_ :
              (Sr.input option * Sr.input_request)
              Environment.Error_monad.tzresult) =
        Proof.valid
          ~pvm:(module PVM)
          ~metadata
          game.Game.inbox_snapshot
          game.Game.inbox_level
          game.Game.dal_snapshot
          ~find_dal_parameters:(Dal.Past_parameters.parameters context)
          ~dal_activation_level
          ~is_reveal_enabled
          ~dal_attested_slots_validity_lag
          {proof with Proof.pvm_step}
      in
      return_unit

let check_refute_proof context ~rollup ~stakers ~choice ~index_proofs
    ~(proof : Proof.serialized Proof.t) :
    unit Environment.Error_monad.tzresult Lwt.t =
  let open Environment.Error_monad.Lwt_result_syntax in
  let* _ctxt, game_opt =
    Sr.Refutation_storage.find_game context rollup stakers
  in
  match game_opt with
  | None -> return_unit
  | Some game -> (
      let* () = check_inbox_proof_level proof in
      let*? () = check_payload_indices ~index_proofs proof in
      let* () = check_proof context rollup game proof in
      match game.Game.game_state with
      | Game.Dissecting {dissection; _} -> (
          match find_section_around_choice dissection choice with
          | Error `Choice_not_found -> return_unit
          | Ok (start_chunk, stop_chunk) -> (
              let dist =
                Sr.Tick.distance
                  start_chunk.Sr.Dissection_chunk.tick
                  stop_chunk.Sr.Dissection_chunk.tick
              in
              if Z.compare dist Z.one > 0 then
                tzfail
                  (Sc_rollup_proof_on_multi_tick_section_during_dissecting dist)
              else
                (* Even on a single-tick section, a proof whose agreed start
                   state is absent ([None]) is indefensible: no PVM proof can
                   start from [None], so both players fail the start-state
                   check, the protocol stores a [Final_move] on the [None]
                   chunk and the game resolves as a [Draw] that burns the
                   honest staker's bond. Reject it like a multi-tick proof. *)
                match start_chunk.Sr.Dissection_chunk.state_hash with
                | None ->
                    tzfail
                      (Sc_rollup_proof_on_missing_start_state_during_dissecting
                         (Sr.Tick.to_z start_chunk.Sr.Dissection_chunk.tick))
                | Some _ -> return_unit))
      | Game.Final_move _ -> return_unit)

(* A smart-rollup refutation [Proof] move whose DAL page proof references a
   [published_level] greater than the level at which the operation is processed
   cannot correspond to a published slot (a legitimate proof references a slot
   published in the past, so [published_level <= current level]), and it makes
   the protocol's page-validity check compute an out-of-range level
   ([Raw_level_repr.add] overflowing into a negative [int32] and tripping an
   assertion in the frozen [lib_protocol], aborting block application).

   Since [lib_protocol] is frozen, this filtering lives in the plugin (used both
   by block validation, [check_block_operation], and by the mempool,
   [Mempool.pre_filter]). The error is [`Temporary] rather than [`Permanent]:
   the operation is not invalid forever -- it is rejected only because its
   [published_level] is greater than the current level, and as the head
   advances that level may be reached, after which the operation could
   become valid. [`Temporary] is the category whose generic classification
   (see [prevalidation.ml]) is [`Branch_delayed], the classification
   [Mempool.pre_filter] assigns here. *)
type Environment.Error_monad.error +=
  | Sc_rollup_refute_dal_proof_future_published_level of {
      published_level : Raw_level.t;
      level : Raw_level.t;
    }
  | Increase_paid_storage_amount_overflow of Z.t

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Temporary
    ~id:
      "block_validation_plugin.sc_rollup_refute_dal_proof_future_published_level"
    ~title:"Sc_rollup refutation with a future DAL published level"
    ~description:
      "A smart-rollup refutation proof references a DAL page whose published \
       level is greater than the current level. Applying it would overflow an \
       internal level computation, so it is rejected by the plugin."
    ~pp:(fun ppf (published_level, level) ->
      Format.fprintf
        ppf
        "Smart-rollup refutation rejected: its DAL page proof has \
         published_level %a, which is greater than the current level %a."
        Raw_level.pp
        published_level
        Raw_level.pp
        level)
    Data_encoding.(
      obj2
        (req "published_level" Raw_level.encoding)
        (req "level" Raw_level.encoding))
    (function
      | Sc_rollup_refute_dal_proof_future_published_level
          {published_level; level} ->
          Some (published_level, level)
      | _ -> None)
    (fun (published_level, level) ->
      Sc_rollup_refute_dal_proof_future_published_level {published_level; level}) ;
  register_error_kind
    `Permanent
    ~id:"block_validation_plugin.increase_paid_storage_amount_overflow"
    ~title:"increase paid storage amount overflow"
    ~description:
      "The storage amount for the operation Increase_paid_storage does not fit \
       in an int64."
    ~pp:(fun ppf amount ->
      Format.fprintf
        ppf
        "Increase_paid_storage operation error: amount %s does not fit an int64"
        (Z.to_string amount))
    Data_encoding.(obj1 (req "amount" (conv Z.to_string Z.of_string string)))
    (function
      | Increase_paid_storage_amount_overflow amount -> Some amount | _ -> None)
    (fun amount -> Increase_paid_storage_amount_overflow amount)

(* The DAL page [published_level] targeted by a refutation [Proof] move, if the
   move carries a DAL page proof. *)
let dal_page_published_level (refutation : Sc_rollup.Game.refutation) =
  match refutation with
  | Sc_rollup.Game.Move
      {
        step =
          Sc_rollup.Game.Proof
            {
              input_proof =
                Some
                  (Sc_rollup.Proof.Reveal_proof
                     (Sc_rollup.Proof.Dal_page_proof {page_id; _}));
              _;
            };
        _;
      } ->
      Some page_id.slot_id.published_level
  | _ -> None

let manager_op_future_dal : type kind.
    level:Raw_level.t -> kind manager_operation -> Raw_level.t option =
 fun ~level operation ->
  match operation with
  | Sc_rollup_refute {refutation; _} -> (
      match dal_page_published_level refutation with
      | Some published_level when Raw_level.(published_level > level) ->
          Some published_level
      | _ -> None)
  | _ -> None

(* [find_future_dal_refute ~level op] returns [Some published_level] when [op]
   contains a [Sc_rollup_refute] whose DAL page proof targets a [published_level]
   greater than [level]. [level] is the level at which the operation is
   processed: the block's own level for block validation, and [head + 1] (the
   block the operation would be baked into) for the mempool. *)
let find_future_dal_refute ~level
    ({protocol_data = Operation_data {contents; _}; _} : packed_operation) =
  List.find_map
    (function
      | Contents (Manager_operation {operation; _}) ->
          manager_op_future_dal ~level operation
      | Contents _ -> None)
    (Operation.to_list (Contents_list contents))

let check_double_baking_evidence (bh1 : Block_header.t) (bh2 : Block_header.t) =
  let open Result_syntax in
  let unsigned_header1 =
    Data_encoding.Binary.to_bytes_opt
      Block_header.unsigned_encoding
      (bh1.shell, bh1.protocol_data.contents)
  in
  let unsigned_header2 =
    Data_encoding.Binary.to_bytes_opt
      Block_header.unsigned_encoding
      (bh2.shell, bh2.protocol_data.contents)
  in
  match (unsigned_header1, unsigned_header2) with
  | Some unsigned_header1, Some unsigned_header2 ->
      if Bytes.equal unsigned_header1 unsigned_header2 then
        Error (Invalid_double_baking_evidence {level = bh1.shell.level})
      else return_unit
  | None, _ | _, None ->
      Error (Invalid_double_baking_evidence {level = bh1.shell.level})

let check_increase_paid_storage_amount amount_in_bytes :
    unit Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  if Z.fits_int64 amount_in_bytes then return_unit
  else shell_fail (Increase_paid_storage_amount_overflow amount_in_bytes)

type Environment.Error_monad.error +=
  | Stake_amount_too_small of Tez.t
  | Delegate_cannot_stake_during_finalization_of_slashed_period

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Permanent
    ~id:"block_validation_plugin.stake_amount_too_small"
    ~title:"Stake amount too small"
    ~description:"The staked amount is too small"
    ~pp:(fun ppf amount ->
      Format.fprintf
        ppf
        "Stake operation error: the amount %a is too small, the staker would \
         not receive any stake from this operation."
        Tez.pp
        amount)
    Data_encoding.(obj1 (req "amount" Tez.encoding))
    (function Stake_amount_too_small amount -> Some amount | _ -> None)
    (fun amount -> Stake_amount_too_small amount)

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Permanent
    ~id:
      "block_validation_plugin.delegate_cannot_stake_during_finalization_of_slashed_period"
    ~title:"Delegate cannot stake until finalization of slashed period"
    ~description:
      "A delegate cannot stake while it has been slashed and related unstake \
       requests cannot be finalized."
    ~pp:(fun ppf _ ->
      Format.fprintf
        ppf
        "A delegate cannot stake while it has been slashed and related unstake \
         requests cannot be finalized.")
    Data_encoding.unit
    (function
      | Delegate_cannot_stake_during_finalization_of_slashed_period -> Some ()
      | _ -> None)
    (fun () -> Delegate_cannot_stake_during_finalization_of_slashed_period)

(* [stake_mints_no_pseudotoken context ~staker ~amount] returns [true] when a
   [`stake`] of [amount] by [staker] would mint no staking pseudotoken. *)
let stake_mints_no_pseudotoken context ~staker ~amount :
    bool Environment.Error_monad.tzresult Lwt.t =
  let open Environment.Error_monad in
  let open Lwt_result_syntax in
  if Tez.(amount <= zero) then return false
  else
    let* delegate_opt =
      Contract.Delegate.find context (Contract.Implicit staker)
    in
    match delegate_opt with
    | None -> return false
    | Some delegate ->
        if Signature.Public_key_hash.(staker = delegate) then return false
        else
          let* frozen_deposits_pseudotokens =
            Staking_pseudotokens.For_RPC.get_frozen_deposits_pseudotokens
              context
              ~delegate
          in
          let pseudotokens =
            Staking_pseudotoken.Internal_for_tests.to_z
              frozen_deposits_pseudotokens
          in
          if Z.(equal pseudotokens zero) then
            (* Uninitialized: rate = 1 *)
            return false
          else
            let* frozen_deposits_staked_tez =
              Staking_pseudotokens.For_RPC.get_frozen_deposits_staked_tez
                context
                ~delegate
            in
            if Tez.(frozen_deposits_staked_tez = zero) then
              (* Pseudotokens != 0, but total stake = 0
                 => fully slashed delegate, op rejected elsewhere *)
              return false
            else
              (* rate = total_pseudotokens / frozen_deposits_staked_tez.
                 Issued pseudotokens = floor (amount * rate).
                 reject iff amount * rate < 1 *)
              return
                Z.(
                  lt
                    (mul pseudotokens (of_int64 (Tez.to_mutez amount)))
                    (of_int64 (Tez.to_mutez frozen_deposits_staked_tez)))

let delegate_stake_while_slashed_in_current_finalization_delay context ~delegate
    =
  let open Environment.Error_monad in
  let open Lwt_result_syntax in
  let* is_delegate = Contract.is_delegate context delegate in
  if is_delegate then
    (* Slashing history is available only at raw context *)
    let raw_context = Alpha_context.Internal_for_tests.to_raw context in
    let delay = Constants.unstake_finalization_delay context + 1 in
    let current_cycle = Cycle_storage.current raw_context in
    let slashed_cycle_horizon =
      Cycle_repr.sub current_cycle delay
      |> Option.value ~default:Cycle_repr.root
    in
    let* slashing_history =
      Storage.Slashed_deposits.find raw_context delegate
    in
    match slashing_history with
    | None -> return_false
    | Some slashing_history ->
        (* Check if there is a cycle between `current_cycle -
         unstake_finalization_delay - 1` and `current_cycle` where the
         delegate has been slashed. If so, the stake operation is rejected.
         Note that the history is sorted in ascendent order of the cycles, so
         we need to go through the whole list before finding a potential culprit. *)
        let slashed_during_unstake_delay =
          List.exists
            (fun (cycle, _) -> Cycle_repr.(cycle >= slashed_cycle_horizon))
            slashing_history
        in
        return slashed_during_unstake_delay
  else return_false

let check_execute_outbox_message context ~rollup ~output_proof =
  let open Environment.Error_monad.Lwt_result_syntax in
  (* [config] is empty for outbox proof verification per protocol semantics *)
  protect_use_wasm_pvm ~config:[] context rollup @@ fun (module PVM) ->
  let output_proof =
    Data_encoding.Binary.of_string_exn PVM.output_proof_encoding output_proof
  in
  let*! _ = PVM.verify_output_proof output_proof in
  return_unit

let check_block_operation {context}
    ({protocol_data = Operation_data {contents; _}; _} as packed_op :
      packed_operation) :
    block_validation_state Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  let contents_list = Operation.to_list (Contents_list contents) in
  let current_level = (Level.current context).level in
  (* Reject un-appliable smart-rollup refutations (see
     [find_future_dal_refute]) before the protocol-specific checks below. *)
  let* () =
    match find_future_dal_refute ~level:current_level packed_op with
    | Some published_level ->
        shell_fail
          (Sc_rollup_refute_dal_proof_future_published_level
             {published_level; level = current_level})
    | None -> return_unit
  in
  let* () =
    List.iter_es
      (function
        | Contents
            (Manager_operation
               {
                 operation =
                   Sc_rollup_execute_outbox_message {rollup; output_proof; _};
                 _;
               }) ->
            let* () =
              check_execute_outbox_message context ~rollup ~output_proof
              |> Lwt.map Environment.wrap_tzresult
            in
            return_unit
        | _ -> return_unit)
      contents_list
  in
  let games_started = games_started_in_operation contents_list in
  (* Only a batch with a refutation Proof move needs its index proofs. *)
  let index_proofs = lazy (index_proofs_of_batch packed_op) in
  let* () =
    List.iter_es
      (function
        | Contents
            (Manager_operation
               {
                 source;
                 operation = Sc_rollup_refute {rollup; opponent; refutation};
                 _;
               }) -> (
            match refutation with
            | Game.Start _ ->
                (* No state-dependent check reads the game for a [Start], and
                   before the game exists both players may legitimately send
                   one. *)
                return_unit
            | Game.Move {step; choice} -> (
                let stakers = Game.Index.make source opponent in
                let* () =
                  check_refute_move_expected context rollup stakers source
                in
                match step with
                | Game.Proof proof ->
                    if
                      List.exists
                        (game_key_equal (rollup, stakers))
                        games_started
                    then
                      (* This operation also opens the game, so the move is
                         applied to one the start-of-block context never
                         held. *)
                      shell_fail
                        (Sc_rollup_proof_on_game_started_in_same_operation
                           {rollup; stakers})
                    else
                      (* The move is the one the game expects, and it is that
                         same game, so the start-of-block state is the state it
                         will be applied to. *)
                      check_refute_proof
                        context
                        ~rollup
                        ~stakers
                        ~choice
                        ~index_proofs:(Lazy.force index_proofs)
                        ~proof
                      |> Lwt.map Environment.wrap_tzresult
                | Game.Dissection _ -> return_unit))
        | Contents
            (Double_baking_evidence {bh1 : Block_header.t; bh2 : Block_header.t})
          -> (
            match check_double_baking_evidence bh1 bh2 with
            | Ok () -> return_unit
            | Error err -> shell_fail err)
        | Contents
            (Manager_operation
               {
                 source = _;
                 operation =
                   Increase_paid_storage {amount_in_bytes; destination = _};
                 _;
               }) ->
            check_increase_paid_storage_amount amount_in_bytes
        | Contents
            (Manager_operation
               {
                 source;
                 operation =
                   Transaction
                     {amount; destination = Implicit staker; entrypoint; _};
                 _;
               })
          when Entrypoint.(entrypoint = stake)
               && Signature.Public_key_hash.(source = staker) ->
            let* mints_no_pseudotoken =
              stake_mints_no_pseudotoken context ~staker ~amount
              |> Lwt.map Environment.wrap_tzresult
            in
            if mints_no_pseudotoken then
              shell_fail (Stake_amount_too_small amount)
            else
              let* delegate_stake_from_slashed_unstake =
                delegate_stake_while_slashed_in_current_finalization_delay
                  context
                  ~delegate:staker
                |> Lwt.map Environment.wrap_tzresult
              in
              if delegate_stake_from_slashed_unstake then
                shell_fail
                  Delegate_cannot_stake_during_finalization_of_slashed_period
              else return_unit
        | _ -> return_unit)
      contents_list
  in
  return {context}
