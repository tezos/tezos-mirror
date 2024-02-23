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

let check_not_tz4 : Signature.Public_key_hash.t -> unit tzresult =
  let open Result_syntax in
  function
  | Bls tz4 -> tzfail (Forbidden_tz4_delegate tz4)
  | Ed25519 _ | Secp256k1 _ | P256 _ -> return_unit

let find = Storage.Contract.Delegate.find

type delegate_status =
  | Delegate
  | Delegated of Signature.Public_key_hash.t
  | Undelegated

let get_delegate_status ctxt pkh =
  let open Lwt_result_syntax in
  let+ delegate = find ctxt (Contract_repr.Implicit pkh) in
  match delegate with
  | None -> Undelegated
  | Some delegate when Signature.Public_key_hash.(delegate = pkh) -> Delegate
  | Some delegate -> Delegated delegate

let is_delegate ctxt pkh =
  let open Lwt_result_syntax in
  let+ find_res = get_delegate_status ctxt pkh in
  match find_res with Delegate -> true | Delegated _ | Undelegated -> false

let init ctxt contract delegate =
  let open Lwt_result_syntax in
  let*? () = check_not_tz4 delegate in
  let* ctxt = Storage.Contract.Delegate.init ctxt contract delegate in
  let delegate_contract = Contract_repr.Implicit delegate in
  let*! ctxt =
    Storage.Contract.Delegated.add (ctxt, delegate_contract) contract
  in
  return ctxt

let unlink ctxt contract =
  let open Lwt_result_syntax in
  let* delegate_opt = Storage.Contract.Delegate.find ctxt contract in
  match delegate_opt with
  | None -> return ctxt
  | Some delegate ->
      let delegate_contract = Contract_repr.Implicit delegate in
      let*! ctxt =
        Storage.Contract.Delegated.remove (ctxt, delegate_contract) contract
      in
      return ctxt

let delete ctxt contract =
  let open Lwt_result_syntax in
  let* ctxt = unlink ctxt contract in
  let*! ctxt = Storage.Contract.Delegate.remove ctxt contract in
  return ctxt

let set ctxt contract delegate =
  let open Lwt_result_syntax in
  let*? () = check_not_tz4 delegate in
  let* ctxt = unlink ctxt contract in
  let*! ctxt = Storage.Contract.Delegate.add ctxt contract delegate in
  let delegate_contract = Contract_repr.Implicit delegate in
  let*! ctxt =
    Storage.Contract.Delegated.add (ctxt, delegate_contract) contract
  in
  return ctxt

let delegated_contracts ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  Storage.Contract.Delegated.elements (ctxt, contract)
