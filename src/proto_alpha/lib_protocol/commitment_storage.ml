(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let exists = Storage.Commitments.mem

let committed_amount ctxt bpkh =
  let open Lwt_result_syntax in
  let+ balance = Storage.Commitments.find ctxt bpkh in
  Option.value ~default:Tez_repr.zero balance

let increase_commitment_only_call_from_token ctxt bpkh amount =
  let open Lwt_result_syntax in
  if Tez_repr.(amount = zero) then return ctxt
  else
    let* balance = committed_amount ctxt bpkh in
    let*? new_balance = Tez_repr.(amount +? balance) in
    let*! result = Storage.Commitments.add ctxt bpkh new_balance in
    return result

let decrease_commitment_only_call_from_token ctxt bpkh amount =
  let open Lwt_result_syntax in
  let* balance = committed_amount ctxt bpkh in
  let*? new_balance = Tez_repr.(balance -? amount) in
  let*! result =
    if Tez_repr.(new_balance = Tez_repr.zero) then
      Storage.Commitments.remove ctxt bpkh
    else Storage.Commitments.add ctxt bpkh new_balance
  in
  return result
