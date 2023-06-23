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

type error += (* `Permanent *) Forbidden_tz4_delegate of Bls.Public_key_hash.t

let () =
  register_error_kind
    `Branch
    ~id:"delegate.forbidden_tz4"
    ~title:"Forbidden delegate"
    ~description:"Delegates are forbidden to be tz4 (BLS) accounts."
    ~pp:(fun ppf implicit ->
      Format.fprintf
        ppf
        "The delegate %a is forbidden as it is a BLS public key hash."
        Bls.Public_key_hash.pp
        implicit)
    Data_encoding.(obj1 (req "delegate" Bls.Public_key_hash.encoding))
    (function Forbidden_tz4_delegate d -> Some d | _ -> None)
    (fun d -> Forbidden_tz4_delegate d)

let check_not_tz4 : Signature.Public_key_hash.t -> unit tzresult = function
  | Bls tz4 -> error (Forbidden_tz4_delegate tz4)
  | Ed25519 _ | Secp256k1 _ | P256 _ -> Ok ()

let find = Storage.Contract.Delegate.find

let is_delegate ctxt pkh =
  let open Lwt_result_syntax in
  let+ delegate = find ctxt (Contract_repr.Implicit pkh) in
  match delegate with
  | None -> false
  | Some delegate -> Signature.Public_key_hash.(delegate = pkh)

let init ctxt contract delegate =
  check_not_tz4 delegate >>?= fun () ->
  Storage.Contract.Delegate.init ctxt contract delegate >>=? fun ctxt ->
  let delegate_contract = Contract_repr.Implicit delegate in
  Storage.Contract.Delegated.add (ctxt, delegate_contract) contract >|= ok

let unlink ctxt contract =
  Storage.Contract.Delegate.find ctxt contract >>=? function
  | None -> return ctxt
  | Some delegate ->
      let delegate_contract = Contract_repr.Implicit delegate in
      Storage.Contract.Delegated.remove (ctxt, delegate_contract) contract
      >|= ok

let delete ctxt contract =
  unlink ctxt contract >>=? fun ctxt ->
  Storage.Contract.Delegate.remove ctxt contract >|= ok

let set ctxt contract delegate =
  check_not_tz4 delegate >>?= fun () ->
  unlink ctxt contract >>=? fun ctxt ->
  Storage.Contract.Delegate.add ctxt contract delegate >>= fun ctxt ->
  let delegate_contract = Contract_repr.Implicit delegate in
  Storage.Contract.Delegated.add (ctxt, delegate_contract) contract >|= ok

let delegated_contracts ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  Storage.Contract.Delegated.elements (ctxt, contract)
