(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let find = Storage.Contract.Delegate.find

let remove_contract_stake ctxt contract amount =
  find ctxt contract >>=? function
  | None -> return ctxt
  | Some delegate -> Stake_storage.remove_stake ctxt delegate amount

let add_contract_stake ctxt contract amount =
  find ctxt contract >>=? function
  | None -> return ctxt
  | Some delegate -> Stake_storage.add_stake ctxt delegate amount

(* A delegate is registered if its "implicit account" delegates to itself. *)
let registered c delegate =
  Storage.Contract.Delegate.find c (Contract_repr.implicit_contract delegate)
  >|=? function
  | Some current_delegate ->
      Signature.Public_key_hash.equal delegate current_delegate
  | None -> false

let link c contract delegate =
  Storage.Contract.Spendable_balance.get c contract >>=? fun balance ->
  Stake_storage.add_stake c delegate balance >>=? fun c ->
  Storage.Contract.Delegated.add
    (c, Contract_repr.implicit_contract delegate)
    contract
  >|= ok

let unlink c contract =
  Storage.Contract.Delegate.find c contract >>=? function
  | None -> return c
  | Some delegate ->
      Storage.Contract.Spendable_balance.get c contract >>=? fun balance ->
      (* Removes the balance of the contract from the delegate *)
      Stake_storage.remove_stake c delegate balance >>=? fun c ->
      Storage.Contract.Delegated.remove
        (c, Contract_repr.implicit_contract delegate)
        contract
      >|= ok

let init ctxt contract delegate =
  Storage.Contract.Delegate.init ctxt contract delegate >>=? fun ctxt ->
  link ctxt contract delegate

let delete ctxt contract =
  unlink ctxt contract >>=? fun ctxt ->
  Storage.Contract.Delegate.remove ctxt contract >|= ok

let set ctxt contract delegate =
  unlink ctxt contract >>=? fun ctxt ->
  Storage.Contract.Delegate.add ctxt contract delegate >>= fun ctxt ->
  link ctxt contract delegate

let delegated_contracts ctxt delegate =
  let contract = Contract_repr.implicit_contract delegate in
  Storage.Contract.Delegated.elements (ctxt, contract)
