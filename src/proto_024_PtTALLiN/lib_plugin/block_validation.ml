(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
module Delegate_set = Signature.Public_key_hash.Set
module Sr = Sc_rollup
module Game = Sr.Game

(* [context] is the context at the START of the block: it is not updated as
   operations are applied. The proof check below is therefore only sound if no
   earlier operation in the same block may have changed the game state it reads
   — hence [seen_games]: at most one operation per refutation game per block. *)
type block_validation_state = {
  context : context;
  delegates : Delegate_set.t;
  seen_games : (Sr.Address.t * Game.Index.t) list;
}

let init_block_validation_state validation_state : block_validation_state =
  let context = Validate.get_initial_ctxt validation_state in
  {context; delegates = Delegate_set.empty; seen_games = []}

let shell_fail err : 'a Environment.Error_monad.shell_tzresult Lwt.t =
  Lwt.return_error [Environment.wrap_tzerror err]

type protected_action =
  | Delegation_change of {
      source : Signature.Public_key_hash.t;
      current_delegate : Signature.Public_key_hash.t;
      new_delegate : Signature.Public_key_hash.t option;
    }
  | Unstake of {
      source : Signature.Public_key_hash.t;
      current_delegate : Signature.Public_key_hash.t;
    }

type Environment.Error_monad.error +=
  | Forbidden_delegation_change of {
      source : Signature.Public_key_hash.t;
      current_delegate : Signature.Public_key_hash.t;
      new_delegate : Signature.Public_key_hash.t option;
    }
  | Forbidden_unstake of {
      source : Signature.Public_key_hash.t;
      current_delegate : Signature.Public_key_hash.t;
    }
  | Multiple_protected_staking_actions_for_delegate of {
      source : Signature.Public_key_hash.t;
      current_delegate : Signature.Public_key_hash.t;
      action : string;
    }
  | Sc_rollup_proof_on_multi_tick_section_during_dissecting of Z.t
  | Sc_rollup_multiple_operations_for_game_in_block of {
      rollup : Sr.Address.t;
      stakers : Game.Index.t;
    }

let () =
  let open Environment.Error_monad in
  register_error_kind
    `Branch
    ~id:"block_validation_plugin.forbidden_delegation_change"
    ~title:"Forbidden delegation change"
    ~description:"Forbidden delegate change"
    ~pp:(fun ppf (source, current_delegate, new_delegate) ->
      Format.fprintf
        ppf
        "Delegation change from %a away from %a to %a was rejected by the \
         block validation plugin filter"
        Signature.Public_key_hash.pp
        source
        Signature.Public_key_hash.pp
        current_delegate
        (Format.pp_print_option Signature.Public_key_hash.pp)
        new_delegate)
    Data_encoding.(
      obj3
        (req "source" Signature.Public_key_hash.encoding)
        (req "current_delegate" Signature.Public_key_hash.encoding)
        (opt "new_delegate" Signature.Public_key_hash.encoding))
    (function
      | Forbidden_delegation_change {source; current_delegate; new_delegate} ->
          Some (source, current_delegate, new_delegate)
      | _ -> None)
    (fun (source, current_delegate, new_delegate) ->
      Forbidden_delegation_change {source; current_delegate; new_delegate}) ;
  register_error_kind
    `Branch
    ~id:"block_validation_plugin.forbidden_unstake"
    ~title:"Forbidden unstake"
    ~description:"Forbidden unstake operation"
    ~pp:(fun ppf (source, current_delegate) ->
      Format.fprintf
        ppf
        "Unstake from %a at delegate %a was rejected by the block validation \
         plugin filter"
        Signature.Public_key_hash.pp
        source
        Signature.Public_key_hash.pp
        current_delegate)
    Data_encoding.(
      obj2
        (req "source" Signature.Public_key_hash.encoding)
        (req "current_delegate" Signature.Public_key_hash.encoding))
    (function
      | Forbidden_unstake {source; current_delegate} ->
          Some (source, current_delegate)
      | _ -> None)
    (fun (source, current_delegate) ->
      Forbidden_unstake {source; current_delegate}) ;
  register_error_kind
    `Branch
    ~id:
      "block_validation_plugin.multiple_protected_staking_actions_for_delegate"
    ~title:"Multiple protected staking actions for a forbidden delegate"
    ~description:
      "A block contains more than one protected staking action for the same \
       forbidden delegate."
    ~pp:(fun ppf (source, current_delegate, action) ->
      Format.fprintf
        ppf
        "%s from %a was rejected because delegate %a already had a protected \
         staking action earlier in the same block"
        action
        Signature.Public_key_hash.pp
        source
        Signature.Public_key_hash.pp
        current_delegate)
    Data_encoding.(
      obj3
        (req "source" Signature.Public_key_hash.encoding)
        (req "current_delegate" Signature.Public_key_hash.encoding)
        (req "action" string))
    (function
      | Multiple_protected_staking_actions_for_delegate
          {source; current_delegate; action} ->
          Some (source, current_delegate, action)
      | _ -> None)
    (fun (source, current_delegate, action) ->
      Multiple_protected_staking_actions_for_delegate
        {source; current_delegate; action}) ;
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
      Sc_rollup_multiple_operations_for_game_in_block {rollup; stakers})

let is_unstake_transaction : type kind.
    source:Signature.Public_key_hash.t -> kind manager_operation -> bool =
 fun ~source -> function
  | Transaction {destination = Contract.Implicit destination; entrypoint; _} ->
      Signature.Public_key_hash.equal source destination
      && Entrypoint.(entrypoint = unstake)
  | _ -> false

let external_staker_delegate ctxt source =
  let open Lwt_result_syntax in
  let* delegate_status =
    Contract.get_delegate_status ctxt source
    |> Lwt.map Environment.wrap_tzresult
  in
  match delegate_status with
  | Delegate | Undelegated -> return_none
  | Delegated current_delegate -> return_some current_delegate

let source_has_staking_numerator raw_ctxt source =
  let open Lwt_result_syntax in
  let delegator = Contract_repr.Implicit source in
  let* staking_numerator =
    Staking_pseudotokens_storage.For_RPC.staking_pseudotokens_balance
      raw_ctxt
      ~delegator
    |> Lwt.map Environment.wrap_tzresult
  in
  return Staking_pseudotoken_repr.(staking_numerator <> zero)

let delegate_has_zero_external_staked raw_ctxt delegate =
  let open Lwt_result_syntax in
  let* staking_balance =
    Stake_storage.get_full_staking_balance raw_ctxt delegate
    |> Lwt.map Environment.wrap_tzresult
  in
  let external_staked =
    Full_staking_balance_repr.staked_frozen staking_balance
  in
  return Tez_repr.(external_staked = zero)

let classify_protected_action : type kind.
    Alpha_context.t ->
    source:Signature.Public_key_hash.t ->
    kind manager_operation ->
    protected_action option Environment.Error_monad.shell_tzresult Lwt.t =
 fun ctxt ~source operation ->
  let open Lwt_result_syntax in
  match operation with
  | Delegation new_delegate ->
      let* current_delegate_opt = external_staker_delegate ctxt source in
      let action =
        Option.bind current_delegate_opt (fun current_delegate ->
            Some (Delegation_change {source; current_delegate; new_delegate}))
      in
      return action
  | _ when is_unstake_transaction ~source operation ->
      let* current_delegate_opt = external_staker_delegate ctxt source in
      return
        (Option.map
           (fun current_delegate -> Unstake {source; current_delegate})
           current_delegate_opt)
  | _ -> return_none

let action_name = function
  | Delegation_change _ -> "Delegation"
  | Unstake _ -> "Unstake"

let was_slashed raw_ctxt ~current_delegate =
  let open Lwt_result_syntax in
  let* slashing_history =
    Storage.Slashed_deposits.find raw_ctxt current_delegate
  in
  return @@ Option.is_some slashing_history

let reject_if_orphaned_state raw_ctxt = function
  | Delegation_change {source; current_delegate; new_delegate} ->
      let open Lwt_result_syntax in
      let* has_staking_numerator =
        source_has_staking_numerator raw_ctxt source
      in
      if not has_staking_numerator then return_unit
      else
        let* external_staked_is_zero =
          delegate_has_zero_external_staked raw_ctxt current_delegate
        in
        if external_staked_is_zero then
          shell_fail
            (Forbidden_delegation_change
               {source; current_delegate; new_delegate})
        else return_unit
  | Unstake {source; current_delegate} ->
      let open Lwt_result_syntax in
      let* has_staking_numerator =
        source_has_staking_numerator raw_ctxt source
      in
      if not has_staking_numerator then return_unit
      else
        let* external_staked_is_zero =
          delegate_has_zero_external_staked raw_ctxt current_delegate
        in
        if external_staked_is_zero then
          shell_fail (Forbidden_unstake {source; current_delegate})
        else return_unit

let reject_if_duplicate_slashed_delegate raw_ctxt state action =
  let open Lwt_result_syntax in
  match action with
  | Delegation_change {source; current_delegate; _}
  | Unstake {source; current_delegate} ->
      let* was_slashed =
        Lwt.map Environment.wrap_tzresult
        @@ was_slashed raw_ctxt ~current_delegate
      in
      if not was_slashed then return state
      else if Delegate_set.mem current_delegate state then
        shell_fail
          (Multiple_protected_staking_actions_for_delegate
             {source; current_delegate; action = action_name action})
      else return (Delegate_set.add current_delegate state)

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

let check_block_operation {context; delegates; seen_games}
    ({protocol_data = Operation_data {contents; _}; _} : packed_operation) :
    block_validation_state Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  let raw_ctxt = Alpha_context.Internal_for_tests.to_raw context in
  let* delegates, seen_games =
    List.fold_left_es
      (fun (delegates, seen_games) op ->
        match op with
        | Contents (Manager_operation {source; operation; _}) ->
            let* protected_action_opt =
              classify_protected_action context ~source operation
            in
            let* delegates =
              match protected_action_opt with
              | None -> return delegates
              | Some protected_action ->
                  let* delegates =
                    reject_if_duplicate_slashed_delegate
                      raw_ctxt
                      delegates
                      protected_action
                  in
                  let* () =
                    reject_if_orphaned_state raw_ctxt protected_action
                  in
                  return delegates
            in
            let* seen_games =
              match operation with
              | Sc_rollup_refute {rollup; opponent; refutation} ->
                  let stakers = Game.Index.make source opponent in
                  let* seen_games =
                    check_game_not_seen seen_games rollup stakers
                  in
                  let* () =
                    match refutation with
                    | Game.Move {step = Game.Proof _; choice} ->
                        check_refute_proof context rollup stakers choice
                    | _ -> return_unit
                  in
                  return seen_games
              | _ -> return seen_games
            in
            return (delegates, seen_games)
        | Contents _ -> return (delegates, seen_games))
      (delegates, seen_games)
      (Operation.to_list (Contents_list contents))
  in
  return {context; delegates; seen_games}
