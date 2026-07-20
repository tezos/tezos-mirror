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

(* [context] is the context at the START of the block: it is not updated as
   operations are applied. The proof check below is therefore only sound if no
   earlier operation in the same block may have changed the game state it reads
   — hence [seen_games]: at most one operation per refutation game per block. *)
type block_validation_state = {
  context : context;
  seen_games : (Sr.Address.t * Game.Index.t) list;
}

let init_block_validation_state validation_state : block_validation_state =
  let context = Validate.get_initial_ctxt validation_state in
  {context; seen_games = []}

let shell_fail err : 'a Environment.Error_monad.shell_tzresult Lwt.t =
  Lwt.return_error [Environment.wrap_tzerror err]

type Environment.Error_monad.error +=
  | Sc_rollup_proof_on_multi_tick_section_during_dissecting of Z.t
  | Sc_rollup_multiple_operations_for_game_in_block of {
      rollup : Sr.Address.t;
      stakers : Game.Index.t;
    }
  | Sc_rollup_inbox_proof_claimed_level_mismatch of {
      claimed_level : Raw_level.t;
      proven_level : Raw_level.t;
    }

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
      "block_validation_plugin.sc_rollup_multiple_operations_for_game_in_block"
    ~title:"Multiple operations for the same refutation game in a block"
    ~description:
      "A block contains more than one refutation operation targeting the same \
       smart rollup refutation game. At most one refutation operation per game \
       per block is allowed, so that refutation moves are always checked \
       against an up-to-date game state."
    ~pp:(fun ppf (rollup, stakers) ->
      Format.fprintf
        ppf
        "Multiple operations for the refutation game between %a and %a on \
         rollup %a in the same block"
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
      | Sc_rollup_multiple_operations_for_game_in_block {rollup; stakers} ->
          Some (rollup, stakers)
      | _ -> None)
    (fun (rollup, stakers) ->
      Sc_rollup_multiple_operations_for_game_in_block {rollup; stakers}) ;
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
      Sc_rollup_inbox_proof_claimed_level_mismatch {claimed_level; proven_level})

let game_key_equal (rollup1, stakers1) (rollup2, stakers2) =
  Sr.Address.equal rollup1 rollup2
  && Sr.Staker.equal stakers1.Game.Index.alice stakers2.Game.Index.alice
  && Sr.Staker.equal stakers1.Game.Index.bob stakers2.Game.Index.bob

(* Reject the operation if [seen_games] already contains an operation for the
   game [stakers] on [rollup] in the current block; otherwise record it. *)
let check_game_not_seen seen_games rollup stakers :
    (Sr.Address.t * Game.Index.t) list Environment.Error_monad.shell_tzresult
    Lwt.t =
  if List.exists (game_key_equal (rollup, stakers)) seen_games then
    shell_fail
      (Sc_rollup_multiple_operations_for_game_in_block {rollup; stakers})
  else Lwt_result_syntax.return ((rollup, stakers) :: seen_games)

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
  let open Lwt_result_syntax in
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
                shell_fail
                  (Sc_rollup_inbox_proof_claimed_level_mismatch
                     {claimed_level; proven_level})))
  | _ ->
      (* Not an inbox proof (reveal / first inbox message / no input proof). *)
      return_unit

let check_refute_proof context rollup stakers choice
    (proof : Proof.serialized Proof.t) :
    unit Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* _ctxt, game_opt =
    Sr.Refutation_storage.find_game context rollup stakers
    |> Lwt.map Environment.wrap_tzresult
  in
  match game_opt with
  | None -> return_unit
  | Some game -> (
      let* () = check_inbox_proof_level proof in
      match game.Game.game_state with
      | Game.Dissecting {dissection; _} -> (
          match find_section_around_choice dissection choice with
          | Error `Choice_not_found -> return_unit
          | Ok (start_chunk, stop_chunk) ->
              let dist =
                Sr.Tick.distance
                  start_chunk.Sr.Dissection_chunk.tick
                  stop_chunk.Sr.Dissection_chunk.tick
              in
              if Z.compare dist Z.one > 0 then
                shell_fail
                  (Sc_rollup_proof_on_multi_tick_section_during_dissecting dist)
              else return_unit)
      | Game.Final_move _ -> return_unit)

let check_block_operation {context; seen_games}
    ({protocol_data = Operation_data {contents; _}; _} : packed_operation) :
    block_validation_state Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* seen_games =
    List.fold_left_es
      (fun seen_games op ->
        match op with
        | Contents
            (Manager_operation
               {
                 source;
                 operation = Sc_rollup_refute {rollup; opponent; refutation};
                 _;
               }) ->
            let stakers = Game.Index.make source opponent in
            let* seen_games = check_game_not_seen seen_games rollup stakers in
            let* () =
              match refutation with
              | Game.Move {step = Game.Proof proof; choice} ->
                  check_refute_proof context rollup stakers choice proof
              | _ -> return_unit
            in
            return seen_games
        | _ -> return seen_games)
      seen_games
      (Operation.to_list (Contents_list contents))
  in
  return {context; seen_games}
