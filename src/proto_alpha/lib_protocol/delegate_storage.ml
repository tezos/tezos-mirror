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

let registered = Storage.Delegates.mem

module Contract = struct
  let init ctxt contract delegate =
    Contract_manager_storage.is_manager_key_revealed ctxt delegate
    >>=? fun known_delegate ->
    error_unless known_delegate (Unregistered_delegate delegate) >>?= fun () ->
    registered ctxt delegate >>= fun is_registered ->
    error_unless is_registered (Unregistered_delegate delegate) >>?= fun () ->
    Contract_delegate_storage.init ctxt contract delegate >>=? fun ctxt ->
    Contract_storage.get_balance_and_frozen_bonds ctxt contract
    >>=? fun balance_and_frozen_bonds ->
    Stake_storage.add_stake ctxt delegate balance_and_frozen_bonds

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
        Stake_storage.remove_contract_stake c contract balance_and_frozen_bonds
      in
      let* c = Contract_delegate_storage.set c contract delegate in
      let* c = Stake_storage.add_stake c delegate balance_and_frozen_bonds in
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
      Stake_storage.remove_contract_stake c contract balance_and_frozen_bonds
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
        let* c = Stake_storage.add_stake c delegate balance_and_frozen_bonds in
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

let frozen_deposits ctxt delegate =
  Frozen_deposits_storage.get ctxt (Contract_repr.Implicit delegate)

let spendable_balance ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  Storage.Contract.Spendable_balance.get ctxt contract

let staking_balance ctxt delegate =
  registered ctxt delegate >>= fun is_registered ->
  if is_registered then Stake_storage.get_staking_balance ctxt delegate
  else return Tez_repr.zero

let is_forbidden_delegate ctxt delegate =
  let forbidden_delegates = Raw_context.Consensus.forbidden_delegates ctxt in
  Signature.Public_key_hash.Set.mem delegate forbidden_delegates

let forbid_delegate ctxt delegate =
  let ctxt = Raw_context.Consensus.forbid_delegate ctxt delegate in
  let new_forbidden_delegates =
    Raw_context.Consensus.forbidden_delegates ctxt
  in
  Storage.Tenderbake.Forbidden_delegates.add ctxt new_forbidden_delegates

let load_forbidden_delegates ctxt =
  let open Lwt_result_syntax in
  let* forbidden_delegates_opt =
    Storage.Tenderbake.Forbidden_delegates.find ctxt
  in
  let ctxt =
    match forbidden_delegates_opt with
    | Some forbidden_delegates ->
        Raw_context.Consensus.set_forbidden_delegates ctxt forbidden_delegates
    | None ->
        Raw_context.Consensus.set_forbidden_delegates
          ctxt
          Signature.Public_key_hash.Set.empty
  in
  return ctxt

let set_forbidden_delegates ctxt forbidden_delegates =
  let open Lwt_syntax in
  let* ctxt =
    Storage.Tenderbake.Forbidden_delegates.add ctxt forbidden_delegates
  in
  let ctxt =
    Raw_context.Consensus.set_forbidden_delegates ctxt forbidden_delegates
  in
  return ctxt

let reset_forbidden_delegates ctxt =
  if
    Signature.Public_key_hash.Set.is_empty
      (Raw_context.Consensus.forbidden_delegates ctxt)
  then Lwt.return ctxt
  else set_forbidden_delegates ctxt Signature.Public_key_hash.Set.empty

let full_balance ctxt delegate =
  Staking_pseudotokens_storage.costaking_pseudotokens_balance
    ctxt
    (Contract_repr.Implicit delegate)
  >>=? fun pseudotokens ->
  Staking_pseudotokens_storage.tez_of_frozen_deposits_pseudotokens
    ctxt
    delegate
    pseudotokens
  >>=? fun own_frozen_deposits ->
  let delegate_contract = Contract_repr.Implicit delegate in
  Contract_storage.get_balance_and_frozen_bonds ctxt delegate_contract
  >>=? fun balance_and_frozen_bonds ->
  Lwt.return Tez_repr.(own_frozen_deposits +? balance_and_frozen_bonds)

let delegated_balance ctxt delegate =
  staking_balance ctxt delegate >>=? fun staking_balance ->
  full_balance ctxt delegate >>=? fun self_staking_balance ->
  Lwt.return Tez_repr.(staking_balance -? self_staking_balance)

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
