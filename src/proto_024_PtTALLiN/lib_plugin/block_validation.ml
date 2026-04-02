(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
module Delegate_set = Signature.Public_key_hash.Set

type block_validation_state = {context : context; delegates : Delegate_set.t}

let init_block_validation_state validation_state : block_validation_state =
  let context = Validate.get_initial_ctxt validation_state in
  {context; delegates = Delegate_set.empty}

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
        {source; current_delegate; action})

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

let check_block_operation {context; delegates}
    ({protocol_data = Operation_data {contents; _}; _} : packed_operation) :
    block_validation_state Environment.Error_monad.shell_tzresult Lwt.t =
  let open Lwt_result_syntax in
  let raw_ctxt = Alpha_context.Internal_for_tests.to_raw context in
  let* delegates =
    List.fold_left_es
      (fun delegates op ->
        match op with
        | Contents (Manager_operation {source; operation; _}) ->
            let* protected_action_opt =
              classify_protected_action context ~source operation
            in
            let* state =
              match protected_action_opt with
              | None -> return delegates
              | Some protected_action ->
                  let* state =
                    reject_if_duplicate_slashed_delegate
                      raw_ctxt
                      delegates
                      protected_action
                  in
                  let* () =
                    reject_if_orphaned_state raw_ctxt protected_action
                  in
                  return state
            in
            return state
        | Contents _ -> return delegates)
      delegates
      (Operation.to_list (Contents_list contents))
  in
  return {context; delegates}
