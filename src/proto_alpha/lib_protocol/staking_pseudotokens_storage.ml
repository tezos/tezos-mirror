(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Avoids a stitching.
    Initializes contract's pseudotokens so that 1 pseudotoken = 1 mutez. *)
let get_or_init_frozen_deposits_pseudotokens ctxt contract ~frozen_deposits_tez
    =
  let open Lwt_result_syntax in
  let* frozen_deposits_pseudotokens_opt =
    Storage.Contract.Frozen_deposits_pseudotokens.find ctxt contract
  in
  match frozen_deposits_pseudotokens_opt with
  | None ->
      let initial_pseudotokens =
        Staking_pseudotoken_repr.of_int64_exn
          (Tez_repr.to_mutez frozen_deposits_tez)
      in
      let+ ctxt =
        Storage.Contract.Frozen_deposits_pseudotokens.init
          ctxt
          contract
          initial_pseudotokens
      in
      (ctxt, initial_pseudotokens)
  | Some frozen_deposits_pseudotokens ->
      return (ctxt, frozen_deposits_pseudotokens)

let pseudotokens_of ~frozen_deposits_pseudotokens ~frozen_deposits_tez
    ~tez_amount =
  if Tez_repr.(frozen_deposits_tez = zero) then (
    (* When there are no frozen deposits, starts with 1 pseudotoken = 1 mutez. *)
    assert (Staking_pseudotoken_repr.(frozen_deposits_pseudotokens = zero)) ;
    Staking_pseudotoken_repr.of_int64_exn (Tez_repr.to_mutez tez_amount))
  else
    let frozen_deposits_tez_z =
      Z.of_int64 (Tez_repr.to_mutez frozen_deposits_tez)
    in
    let frozen_deposits_pseudotokens_z =
      Z.of_int64
        (Staking_pseudotoken_repr.to_int64 frozen_deposits_pseudotokens)
    in
    let tez_amount_z = Z.of_int64 (Tez_repr.to_mutez tez_amount) in
    let res_z =
      Z.div
        (Z.mul tez_amount_z frozen_deposits_pseudotokens_z)
        frozen_deposits_tez_z
    in
    Staking_pseudotoken_repr.of_int64_exn (Z.to_int64 res_z)

let tez_of ~frozen_deposits_pseudotokens ~frozen_deposits_tez
    ~pseudotoken_amount =
  let frozen_deposits_tez_z =
    Z.of_int64 (Tez_repr.to_mutez frozen_deposits_tez)
  in
  let frozen_deposits_pseudotokens_z =
    Z.of_int64 (Staking_pseudotoken_repr.to_int64 frozen_deposits_pseudotokens)
  in
  let pseudotoken_amount_z =
    Z.of_int64 (Staking_pseudotoken_repr.to_int64 pseudotoken_amount)
  in
  let res_z =
    Z.div
      (Z.mul frozen_deposits_tez_z pseudotoken_amount_z)
      frozen_deposits_pseudotokens_z
  in
  Tez_repr.of_mutez_exn (Z.to_int64 res_z)

let frozen_deposits_pseudotokens_for_tez_amount ctxt delegate tez_amount =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let* {current_amount = frozen_deposits_tez; initial_amount = _} =
    Frozen_deposits_storage.get ctxt contract
  in
  let+ frozen_deposits_pseudotokens =
    Storage.Contract.Frozen_deposits_pseudotokens.get ctxt contract
  in
  pseudotokens_of ~frozen_deposits_pseudotokens ~frozen_deposits_tez ~tez_amount

let update_frozen_deposits_pseudotokens ~f ctxt delegate =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let* {current_amount = frozen_deposits_tez; initial_amount = _} =
    Frozen_deposits_storage.get ctxt contract
  in
  let* ctxt, frozen_deposits_pseudotokens =
    get_or_init_frozen_deposits_pseudotokens ctxt contract ~frozen_deposits_tez
  in
  let*? new_frozen_deposits_pseudotokens, x =
    f ~frozen_deposits_pseudotokens ~frozen_deposits_tez
  in
  let+ ctxt =
    Storage.Contract.Frozen_deposits_pseudotokens.update
      ctxt
      contract
      new_frozen_deposits_pseudotokens
  in
  (ctxt, x)

let credit_frozen_deposits_pseudotokens_for_tez_amount ctxt delegate tez_amount
    =
  let f ~frozen_deposits_pseudotokens ~frozen_deposits_tez =
    let open Result_syntax in
    let pseudotokens_to_add =
      pseudotokens_of
        ~frozen_deposits_pseudotokens
        ~frozen_deposits_tez
        ~tez_amount
    in
    let+ new_pseudotokens_balance =
      Staking_pseudotoken_repr.(
        pseudotokens_to_add +? frozen_deposits_pseudotokens)
    in
    (new_pseudotokens_balance, pseudotokens_to_add)
  in
  update_frozen_deposits_pseudotokens ~f ctxt delegate

let debit_frozen_deposits_pseudotokens ctxt delegate pseudotoken_amount =
  let f ~frozen_deposits_pseudotokens ~frozen_deposits_tez =
    let open Result_syntax in
    let+ new_pseudotokens_balance =
      Staking_pseudotoken_repr.(
        frozen_deposits_pseudotokens -? pseudotoken_amount)
    in
    let tez_amount =
      tez_of
        ~frozen_deposits_pseudotokens
        ~frozen_deposits_tez
        ~pseudotoken_amount
    in
    (new_pseudotokens_balance, tez_amount)
  in
  update_frozen_deposits_pseudotokens ~f ctxt delegate

let costaking_pseudotokens_balance ctxt contract =
  let open Lwt_result_syntax in
  let+ costaking_pseudotokens_opt =
    Storage.Contract.Costaking_pseudotokens.find ctxt contract
  in
  Option.value ~default:Staking_pseudotoken_repr.zero costaking_pseudotokens_opt

let update_costaking_pseudotokens ~f ctxt contract =
  let open Lwt_result_syntax in
  let* costaking_pseudotokens = costaking_pseudotokens_balance ctxt contract in
  let*? new_costaking_pseudotokens = f costaking_pseudotokens in
  let*! ctxt =
    Storage.Contract.Costaking_pseudotokens.add
      ctxt
      contract
      new_costaking_pseudotokens
  in
  return ctxt

let credit_costaking_pseudotokens ctxt contract pseudotokens_to_add =
  let f current_pseudotokens_balance =
    Staking_pseudotoken_repr.(
      current_pseudotokens_balance +? pseudotokens_to_add)
  in
  update_costaking_pseudotokens ~f ctxt contract

let debit_costaking_pseudotokens ctxt contract pseudotokens_to_subtract =
  let f current_pseudotokens_balance =
    Staking_pseudotoken_repr.(
      current_pseudotokens_balance -? pseudotokens_to_subtract)
  in
  update_costaking_pseudotokens ~f ctxt contract
