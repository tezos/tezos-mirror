(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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

(*
  Some invariants:

  A contract is a delegate <=>
    - it is registered (i.e. in the set {!Storage.Delegates.mem}), and
    - its full staking balance is initialized.

  If a contract is a delegate then :
    - it has no stake in another account, though it may (still) have unstake
        requests from another contract.

  If a contract is not a delegate then:
    - it has no *own* frozen stake (a.k.a. frozen deposits),
    - it has no consensus key.

  Once a contract has become a delegate, it is so forever. There are no ways
  to unregister.
*)

type error +=
  | (* `Permanent *) Unregistered_delegate of Signature.Public_key_hash.t

let () =
  (* Unregistered delegate *)
  register_error_kind
    `Permanent
    ~id:"contract.manager.unregistered_delegate"
    ~title:"Unregistered delegate"
    ~description:"A contract cannot be delegated to an unregistered delegate"
    ~pp:(fun ppf k ->
      Format.fprintf
        ppf
        "The provided public key (with hash %a) is not registered as valid \
         delegate key."
        Signature.Public_key_hash.pp
        k)
    Data_encoding.(obj1 (req "hash" Signature.Public_key_hash.encoding))
    (function Unregistered_delegate k -> Some k | _ -> None)
    (fun k -> Unregistered_delegate k)

type error += No_previous_cycle

let registered = Storage.Delegates.mem

module Contract = struct
  let init ctxt contract delegate =
    let open Lwt_result_syntax in
    let* known_delegate =
      Contract_manager_storage.is_manager_key_revealed ctxt delegate
    in
    let*? () = error_unless known_delegate (Unregistered_delegate delegate) in
    let*! is_registered = registered ctxt delegate in
    let*? () = error_unless is_registered (Unregistered_delegate delegate) in
    let* ctxt = Contract_delegate_storage.init ctxt contract delegate in
    let* balance_and_frozen_bonds =
      Contract_storage.get_balance_and_frozen_bonds ctxt contract
    in
    Stake_storage.add_delegated_stake ctxt delegate balance_and_frozen_bonds

  type error +=
    | (* `Temporary *) Active_delegate
    | (* `Permanent *) Empty_delegate_account of Signature.Public_key_hash.t

  let () =
    register_error_kind
      `Temporary
      ~id:"delegate.already_active"
      ~title:"Delegate already active"
      ~description:"Useless delegate reactivation"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "The delegate is still active, no need to refresh it")
      Data_encoding.empty
      (function Active_delegate -> Some () | _ -> None)
      (fun () -> Active_delegate) ;
    register_error_kind
      `Permanent
      ~id:"delegate.empty_delegate_account"
      ~title:"Empty delegate account"
      ~description:
        "Cannot register a delegate when its implicit account is empty"
      ~pp:(fun ppf delegate ->
        Format.fprintf
          ppf
          "Delegate registration is forbidden when the delegate\n\
          \           implicit account is empty (%a)"
          Signature.Public_key_hash.pp
          delegate)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function Empty_delegate_account c -> Some c | _ -> None)
      (fun c -> Empty_delegate_account c)

  let set_self_delegate c delegate =
    let open Lwt_result_syntax in
    let*! is_registered = registered c delegate in
    if is_registered then
      let* () =
        let* is_inactive = Delegate_activation_storage.is_inactive c delegate in
        fail_unless is_inactive Active_delegate
      in
      Stake_storage.set_active c delegate
    else
      let contract = Contract_repr.Implicit delegate in
      let* pk =
        Contract_manager_storage.get_manager_key
          c
          ~error:(Unregistered_delegate delegate)
          delegate
      in
      let* () =
        let*! is_allocated = Contract_storage.allocated c contract in
        fail_unless is_allocated (Empty_delegate_account delegate)
      in
      let* balance_and_frozen_bonds =
        Contract_storage.get_balance_and_frozen_bonds c contract
      in
      let* c =
        Stake_storage.remove_contract_delegated_stake
          c
          contract
          balance_and_frozen_bonds
      in
      let* c = Contract_delegate_storage.set c contract delegate in
      let* c =
        (* Initializes the full staking balance of [delegate]. *)
        Stake_storage.initialize_delegate
          c
          delegate
          ~delegated:balance_and_frozen_bonds
      in
      let*! c = Storage.Delegates.add c delegate in
      let* c = Delegate_consensus_key.init c delegate pk in
      let* c = Stake_storage.set_active c delegate in
      return c

  type error +=
    | (* `Permanent *) No_deletion of Signature.Public_key_hash.t
    | (* `Temporary *) Current_delegate

  let () =
    register_error_kind
      `Permanent
      ~id:"delegate.no_deletion"
      ~title:"Forbidden delegate deletion"
      ~description:"Tried to unregister a delegate"
      ~pp:(fun ppf delegate ->
        Format.fprintf
          ppf
          "Delegate deletion is forbidden (%a)"
          Signature.Public_key_hash.pp
          delegate)
      Data_encoding.(obj1 (req "delegate" Signature.Public_key_hash.encoding))
      (function No_deletion c -> Some c | _ -> None)
      (fun c -> No_deletion c) ;
    register_error_kind
      `Temporary
      ~id:"delegate.unchanged"
      ~title:"Unchanged delegated"
      ~description:"Contract already delegated to the given delegate"
      ~pp:(fun ppf () ->
        Format.fprintf
          ppf
          "The contract is already delegated to the same delegate")
      Data_encoding.empty
      (function Current_delegate -> Some () | _ -> None)
      (fun () -> Current_delegate)

  let set_delegate c contract delegate =
    let open Lwt_result_syntax in
    let* () =
      match contract with
      | Contract_repr.Originated _ -> return_unit
      | Implicit pkh ->
          let*! is_registered = registered c pkh in
          fail_when is_registered (No_deletion pkh)
    in
    let* () =
      let* current_delegate = Contract_delegate_storage.find c contract in
      match (delegate, current_delegate) with
      | None, None ->
          (* we don't fail in this case in order not to risk breaking
             existing smart contracts. *)
          return_unit
      | Some delegate, Some current_delegate
        when Signature.Public_key_hash.equal delegate current_delegate ->
          tzfail Current_delegate
      | _ -> return_unit
    in
    let* balance_and_frozen_bonds =
      Contract_storage.get_balance_and_frozen_bonds c contract
    in
    let* c =
      Stake_storage.remove_contract_delegated_stake
        c
        contract
        balance_and_frozen_bonds
    in
    match delegate with
    | None ->
        let* c = Contract_delegate_storage.delete c contract in
        return c
    | Some delegate ->
        let* () =
          let*! is_delegate_registered = registered c delegate in
          fail_when
            (not is_delegate_registered)
            (Unregistered_delegate delegate)
        in
        let* c = Contract_delegate_storage.set c contract delegate in
        let* c =
          Stake_storage.add_delegated_stake c delegate balance_and_frozen_bonds
        in
        return c

  let set c contract delegate =
    match (delegate, contract) with
    | Some delegate, Contract_repr.Implicit source
      when Signature.Public_key_hash.equal source delegate ->
        set_self_delegate c delegate
    | _ -> set_delegate c contract delegate
end

let fold = Storage.Delegates.fold

let list = Storage.Delegates.elements

let initial_frozen_deposits ctxt delegate =
  let open Lwt_result_syntax in
  let* stake_opt =
    match Raw_context.find_stake_distribution_for_current_cycle ctxt with
    | Some distribution ->
        return (Signature.Public_key_hash.Map.find delegate distribution)
    | None ->
        (* This branch happens when the stake distribution is not initialized in
           [ctxt], e.g. when RPCs are called or operations are simulated. *)
        let current_cycle = (Raw_context.current_level ctxt).cycle in
        let+ stakes =
          Stake_storage.get_selected_distribution ctxt current_cycle
        in
        List.assoc ~equal:Signature.Public_key_hash.equal delegate stakes
  in
  match stake_opt with
  | None -> return Tez_repr.zero
  | Some {frozen; weighted_delegated = _} -> return frozen

let initial_frozen_deposits_of_previous_cycle ctxt delegate =
  let open Lwt_result_syntax in
  let current_cycle = (Raw_context.current_level ctxt).cycle in
  match Cycle_repr.pred current_cycle with
  | None -> tzfail No_previous_cycle
  | Some previous_cycle -> (
      let+ stakes =
        Stake_storage.get_selected_distribution ctxt previous_cycle
      in
      match
        List.assoc ~equal:Signature.Public_key_hash.equal delegate stakes
      with
      | None -> Tez_repr.zero
      | Some {frozen; weighted_delegated = _} -> frozen)

let current_frozen_deposits ctxt delegate =
  let open Lwt_result_syntax in
  let* full_staking_balance =
    Stake_storage.get_full_staking_balance ctxt delegate
  in
  Lwt.return (Full_staking_balance_repr.total_frozen full_staking_balance)

let frozen_deposits_limit ctxt delegate =
  Storage.Contract.Frozen_deposits_limit.find
    ctxt
    (Contract_repr.Implicit delegate)

let set_frozen_deposits_limit ctxt delegate limit =
  Storage.Contract.Frozen_deposits_limit.add_or_remove
    ctxt
    (Contract_repr.Implicit delegate)
    limit

let spendable_balance ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  Storage.Contract.Spendable_balance.get ctxt contract

let drain ctxt ~delegate ~destination =
  let open Lwt_result_syntax in
  let destination_contract = Contract_repr.Implicit destination in
  let*! is_destination_allocated =
    Contract_storage.allocated ctxt destination_contract
  in
  let delegate_contract = Contract_repr.Implicit delegate in
  let* ctxt, _, balance_updates1 =
    if not is_destination_allocated then
      Fees_storage.burn_origination_fees
        ctxt
        ~storage_limit:(Z.of_int (Constants_storage.origination_size ctxt))
        ~payer:(`Contract delegate_contract)
    else return (ctxt, Z.zero, [])
  in
  let* manager_balance = spendable_balance ctxt delegate in
  let*? one_percent = Tez_repr.(manager_balance /? 100L) in
  let fees = Tez_repr.(max one one_percent) in
  let*? transferred = Tez_repr.(manager_balance -? fees) in
  let* ctxt, balance_updates2 =
    Token.transfer
      ctxt
      (`Contract delegate_contract)
      (`Contract destination_contract)
      transferred
  in
  return
    ( ctxt,
      not is_destination_allocated,
      fees,
      balance_updates1 @ balance_updates2 )

module For_RPC = struct
  let full_balance ctxt delegate =
    let open Lwt_result_syntax in
    let* own_frozen_deposits =
      Staking_pseudotokens_storage.For_RPC.staked_balance
        ctxt
        ~delegate
        ~contract:(Contract_repr.Implicit delegate)
    in
    let* unstaked_frozen =
      let* result =
        Unstake_requests_storage.prepare_finalize_unstake
          ctxt
          ~for_next_cycle_use_only_after_slashing:false
          (Contract_repr.Implicit delegate)
      in
      match result with
      | None -> return Tez_repr.zero
      | Some {finalizable; unfinalizable} ->
          let* unfinalizable_requests =
            Unstake_requests_storage.For_RPC
            .apply_slash_to_unstaked_unfinalizable
              ctxt
              unfinalizable
          in
          let*? sum_unfinalizable =
            List.fold_left_e
              (fun acc (_cycle, tz) -> Tez_repr.(acc +? tz))
              Tez_repr.zero
              unfinalizable_requests
          in
          let*? sum =
            List.fold_left_e
              (fun acc (_, _cycle, tz) -> Tez_repr.(acc +? tz))
              sum_unfinalizable
              finalizable
          in
          return sum
    in
    let*? all_frozen = Tez_repr.(own_frozen_deposits +? unstaked_frozen) in
    let delegate_contract = Contract_repr.Implicit delegate in
    let* balance_and_frozen_bonds =
      Contract_storage.get_balance_and_frozen_bonds ctxt delegate_contract
    in
    let*? sum = Tez_repr.(all_frozen +? balance_and_frozen_bonds) in
    return sum

  let staking_balance ctxt delegate =
    let open Lwt_result_syntax in
    let*! is_registered = registered ctxt delegate in
    if is_registered then
      Stake_storage.For_RPC.get_staking_balance ctxt delegate
    else return Tez_repr.zero

  let min_delegated_in_current_cycle ctxt delegate =
    let open Lwt_result_syntax in
    let current_cycle = (Raw_context.current_level ctxt).cycle in
    let*! is_registered = registered ctxt delegate in
    if is_registered then
      let+ staking_balance =
        Stake_storage.get_full_staking_balance ctxt delegate
      in
      Full_staking_balance_repr.min_delegated_in_cycle
        ~current_cycle
        staking_balance
    else return Tez_repr.zero

  let delegated_balance ctxt delegate =
    let open Lwt_result_syntax in
    let* staking_balance = staking_balance ctxt delegate in
    let* self_staking_balance = full_balance ctxt delegate in
    let*? sum = Tez_repr.(staking_balance -? self_staking_balance) in
    return sum
end
