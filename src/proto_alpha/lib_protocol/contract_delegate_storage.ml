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

(* A delegate is registered if its "implicit account" delegates to itself. *)
let registered c delegate =
  Storage.Contract.Delegate.find c (Contract_repr.implicit_contract delegate)
  >|=? function
  | Some current_delegate ->
      Signature.Public_key_hash.equal delegate current_delegate
  | None -> false

let init ctxt contract delegate =
  Storage.Contract.Delegate.init ctxt contract delegate >>=? fun ctxt ->
  let delegate_contract = Contract_repr.implicit_contract delegate in
  Storage.Contract.Delegated.add (ctxt, delegate_contract) contract >|= ok

let unlink ctxt contract =
  Storage.Contract.Delegate.find ctxt contract >>=? function
  | None -> return ctxt
  | Some delegate ->
      let delegate_contract = Contract_repr.implicit_contract delegate in
      Storage.Contract.Delegated.remove (ctxt, delegate_contract) contract
      >|= ok

let delete ctxt contract =
  unlink ctxt contract >>=? fun ctxt ->
  Storage.Contract.Delegate.remove ctxt contract >|= ok

let set ctxt contract delegate =
  unlink ctxt contract >>=? fun ctxt ->
  Storage.Contract.Delegate.add ctxt contract delegate >>= fun ctxt ->
  let delegate_contract = Contract_repr.implicit_contract delegate in
  Storage.Contract.Delegated.add (ctxt, delegate_contract) contract >|= ok

let delegated_contracts ctxt delegate =
  let contract = Contract_repr.implicit_contract delegate in
  Storage.Contract.Delegated.elements (ctxt, contract)
