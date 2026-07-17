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
  | Invalid_double_baking_evidence of {level : Int32.t}

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
    ~id:"block_validation_plugin.invalid_double_baking_evidence"
    ~title:"invalid double baking evidence"
    ~description:"Invalid double baking evidence."
    ~pp:(fun ppf level ->
      Format.fprintf ppf "Invalid double baking evidence at level %ld" level)
    Data_encoding.(obj1 (req "level" int32))
    (function
      | Invalid_double_baking_evidence {level} -> Some level | _ -> None)
    (fun level -> Invalid_double_baking_evidence {level})

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

let check_refute_proof context rollup stakers choice :
    unit Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* _ctxt, game_opt =
    Sr.Refutation_storage.find_game context rollup stakers
    |> Lwt.map Environment.wrap_tzresult
  in
  match game_opt with
  | None -> return_unit
  | Some game -> (
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
      Sc_rollup_refute_dal_proof_future_published_level {published_level; level})

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

let check_block_operation {context; seen_games}
    ({protocol_data = Operation_data {contents; _}; _} as packed_op :
      packed_operation) :
    block_validation_state Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
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
              | Game.Move {step = Game.Proof _; choice} ->
                  check_refute_proof context rollup stakers choice
              | _ -> return_unit
            in
            return seen_games
        | Contents
            (Double_baking_evidence {bh1 : Block_header.t; bh2 : Block_header.t})
          -> (
            match check_double_baking_evidence bh1 bh2 with
            | Ok () -> return seen_games
            | Error err -> shell_fail err)
        | _ -> return seen_games)
      seen_games
      (Operation.to_list (Contents_list contents))
  in
  return {context; seen_games}
