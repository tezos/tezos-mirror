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
  | (* `Permanent *) No_baker_delegation of Baker_hash.t
  | (* `Temporary *)
      Current_delegate
  | (* `Temporary *)
      Inactive_baker of Baker_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"delegation.no_baker_delegation"
    ~title:"Forbidden baker delegation change"
    ~description:"Tried to change a baker contract delegation"
    ~pp:(fun ppf delegate ->
      Format.fprintf
        ppf
        "Change of delegation status of a baker contract is forbidden (%a)"
        Baker_hash.pp
        delegate)
    Data_encoding.(obj1 (req "baker" Baker_hash.encoding))
    (function No_baker_delegation c -> Some c | _ -> None)
    (fun c -> No_baker_delegation c) ;
  register_error_kind
    `Temporary
    ~id:"delegation.unchanged"
    ~title:"Unchanged delegated"
    ~description:"Contract already delegated to the given baker"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The contract is already delegated to the same baker")
    Data_encoding.empty
    (function Current_delegate -> Some () | _ -> None)
    (fun () -> Current_delegate) ;
  register_error_kind
    `Temporary
    ~id:"delegation.inactive_baker"
    ~title:"Delegate is inactive baker"
    ~description:"The given delegate is inactive baker"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "The given delegate %a is inactive baker"
        Baker_hash.pp
        hash)
    Data_encoding.(obj1 (req "hash" Baker_hash.encoding))
    (function Inactive_baker k -> Some k | _ -> None)
    (fun k -> Inactive_baker k)

let link ctxt contract delegate =
  Storage.Contract.Balance.get ctxt contract
  >>=? fun balance ->
  Roll_storage.Delegate.add_amount ctxt delegate balance
  >>=? fun ctxt ->
  Storage.Contract.Delegate.init ctxt contract delegate
  >>=? fun ctxt ->
  Storage.Baker.Delegators.add (ctxt, delegate) contract >|= ok

let unlink ctxt contract =
  Storage.Contract.Balance.get ctxt contract
  >>=? fun balance ->
  Storage.Contract.Delegate.get_option ctxt contract
  >>=? function
  | None ->
      return ctxt
  | Some delegate ->
      (* Removes the delegate and the balance of the contract from the delegate *)
      Roll_storage.Delegate.remove_amount ctxt delegate balance
      >>=? fun ctxt ->
      Storage.Contract.Delegate.delete ctxt contract
      >>=? fun ctxt ->
      Storage.Baker.Delegators.del (ctxt, delegate) contract >|= ok

let get = Roll_storage.get_contract_delegate

let set_delegate ctxt contract delegate =
  (* check that the delegate is a registered baker *)
  Baker_storage.must_be_registered ctxt delegate
  >>=? fun () ->
  (* check that the delegate is an active baker *)
  Baker_storage.deactivated ctxt delegate
  >>=? fun deactivated ->
  fail_unless (not deactivated) (Inactive_baker delegate)
  >>=? fun () ->
  Storage.Contract.Delegate.get_option ctxt contract
  >>=? (function
         | Some current_delegate
           when Baker_hash.equal delegate current_delegate ->
             fail Current_delegate
         | Some _ ->
             unlink ctxt contract
         | None ->
             return ctxt)
  >>=? fun ctxt -> link ctxt contract delegate

let withdraw_delegate c contract =
  unlink c contract
  >>=? fun c -> Storage.Contract.Delegate.remove c contract >|= ok

let set c contract delegate =
  match Contract_repr.is_baker contract with
  | Some baker ->
      fail (No_baker_delegation baker)
  | None -> (
    match delegate with
    | Some delegate ->
        set_delegate c contract delegate
    | None ->
        withdraw_delegate c contract )

let remove ctxt contract = unlink ctxt contract
