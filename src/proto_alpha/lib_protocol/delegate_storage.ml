(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
  | (* `Permanent *) No_deletion of Signature.Public_key_hash.t
  | (* `Temporary *) Active_delegate
  | (* `Temporary *) Current_delegate
  | (* `Temporary *)
      Empty_delegate_account of Signature.Public_key_hash.t

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
    ~id:"delegate.already_active"
    ~title:"Delegate already active"
    ~description:"Useless delegate reactivation"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The delegate is still active, no need to refresh it")
    Data_encoding.empty
    (function Active_delegate -> Some () | _ -> None)
    (fun () -> Active_delegate) ;
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
    (fun () -> Current_delegate) ;
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

let link c contract delegate =
  Storage.Contract.Balance.get c contract
  >>=? fun balance ->
  Roll_storage.Delegate.add_amount c delegate balance
  >>=? fun c ->
  Storage.Contract.Delegated.add
    (c, Contract_repr.implicit_contract delegate)
    contract
  >|= ok

let unlink c contract =
  Storage.Contract.Balance.get c contract
  >>=? fun balance ->
  Storage.Contract.Delegate.find c contract
  >>=? function
  | None ->
      return c
  | Some delegate ->
      (* Removes the balance of the contract from the delegate *)
      Roll_storage.Delegate.remove_amount c delegate balance
      >>=? fun c ->
      Storage.Contract.Delegated.remove
        (c, Contract_repr.implicit_contract delegate)
        contract
      >|= ok

let known c delegate =
  Storage.Contract.Manager.find c (Contract_repr.implicit_contract delegate)
  >>=? function
  | None | Some (Manager_repr.Hash _) ->
      return_false
  | Some (Manager_repr.Public_key _) ->
      return_true

(* A delegate is registered if its "implicit account" delegates to itself. *)
let registered c delegate =
  Storage.Contract.Delegate.find c (Contract_repr.implicit_contract delegate)
  >|=? function
  | Some current_delegate ->
      Signature.Public_key_hash.equal delegate current_delegate
  | None ->
      false

let get = Roll_storage.get_contract_delegate

let set c contract delegate =
  match delegate with
  | None -> (
      let delete () =
        unlink c contract
        >>=? fun c -> Storage.Contract.Delegate.remove c contract >|= ok
      in
      match Contract_repr.is_implicit contract with
      | Some pkh ->
          (* check if contract is a registered delegate *)
          registered c pkh
          >>=? fun is_registered ->
          if is_registered then fail (No_deletion pkh) else delete ()
      | None ->
          delete () )
  | Some delegate ->
      known c delegate
      >>=? fun known_delegate ->
      registered c delegate
      >>=? fun registered_delegate ->
      let self_delegation =
        match Contract_repr.is_implicit contract with
        | Some pkh ->
            Signature.Public_key_hash.equal pkh delegate
        | None ->
            false
      in
      if (not known_delegate) || not (registered_delegate || self_delegation)
      then fail (Roll_storage.Unregistered_delegate delegate)
      else
        Storage.Contract.Delegate.find c contract
        >>=? (function
               | Some current_delegate
                 when Signature.Public_key_hash.equal delegate current_delegate
                 ->
                   if self_delegation then
                     Roll_storage.Delegate.is_inactive c delegate
                     >>=? function
                     | true -> return_unit | false -> fail Active_delegate
                   else fail Current_delegate
               | None | Some _ ->
                   return_unit)
        >>=? fun () ->
        (* check if contract is a registered delegate *)
        ( match Contract_repr.is_implicit contract with
        | Some pkh ->
            registered c pkh
            >>=? fun is_registered ->
            (* allow self-delegation to re-activate *)
            if (not self_delegation) && is_registered then
              fail (No_deletion pkh)
            else return_unit
        | None ->
            return_unit )
        >>=? fun () ->
        Storage.Contract.Balance.mem c contract
        >>= fun exists ->
        error_when
          (self_delegation && not exists)
          (Empty_delegate_account delegate)
        >>?= fun () ->
        unlink c contract
        >>=? fun c ->
        Storage.Contract.Delegate.add c contract delegate
        >>= fun c ->
        link c contract delegate
        >>=? fun c ->
        if self_delegation then
          Storage.Delegates.add c delegate
          >>= fun c -> Roll_storage.Delegate.set_active c delegate
        else return c

let remove ctxt contract = unlink ctxt contract
