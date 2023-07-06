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

let init_delegate_pseudotokens_from_frozen_deposits_balance ctxt contract =
  let open Lwt_result_syntax in
  let* {current_amount = frozen_deposits_tez; initial_amount = _} =
    Frozen_deposits_storage.get ctxt contract
  in
  let initial_pseudotokens =
    Staking_pseudotoken_repr.of_int64_exn
      (Tez_repr.to_mutez frozen_deposits_tez)
  in
  let* ctxt =
    Storage.Contract.Frozen_deposits_pseudotokens.init
      ctxt
      contract
      initial_pseudotokens
  in
  Storage.Contract.Costaking_pseudotokens.init
    ctxt
    contract
    initial_pseudotokens

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
  if Staking_pseudotoken_repr.(frozen_deposits_pseudotokens = zero) then (
    (* When there are no frozen deposits, starts with 1 mutez = 1 pseudotoken. *)
    assert (Tez_repr.(frozen_deposits_tez = zero)) ;
    Tez_repr.of_mutez_exn (Staking_pseudotoken_repr.to_int64 pseudotoken_amount))
  else
    let frozen_deposits_tez_z =
      Z.of_int64 (Tez_repr.to_mutez frozen_deposits_tez)
    in
    let frozen_deposits_pseudotokens_z =
      Z.of_int64
        (Staking_pseudotoken_repr.to_int64 frozen_deposits_pseudotokens)
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

let tez_of_frozen_deposits_pseudotokens ctxt delegate pseudotoken_amount =
  let open Lwt_result_syntax in
  if Staking_pseudotoken_repr.(pseudotoken_amount = zero) then
    return Tez_repr.zero
  else
    let contract = Contract_repr.Implicit delegate in
    let* frozen_deposits_pseudotokens_opt =
      Storage.Contract.Frozen_deposits_pseudotokens.find ctxt contract
    in
    match frozen_deposits_pseudotokens_opt with
    | Some frozen_deposits_pseudotokens
      when Staking_pseudotoken_repr.(frozen_deposits_pseudotokens <> zero) ->
        let+ {current_amount = frozen_deposits_tez; initial_amount = _} =
          Frozen_deposits_storage.get ctxt contract
        in
        tez_of
          ~frozen_deposits_pseudotokens
          ~frozen_deposits_tez
          ~pseudotoken_amount
    | _ ->
        return
          (Tez_repr.of_mutez_exn
             (Staking_pseudotoken_repr.to_int64 pseudotoken_amount))

let update_frozen_deposits_pseudotokens ~f ctxt delegate =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let* {current_amount = frozen_deposits_tez; initial_amount = _} =
    Frozen_deposits_storage.get ctxt contract
  in
  let* frozen_deposits_pseudotokens_opt =
    Storage.Contract.Frozen_deposits_pseudotokens.find ctxt contract
  in
  let* ctxt, frozen_deposits_pseudotokens =
    match frozen_deposits_pseudotokens_opt with
    | Some frozen_deposits_pseudotokens
      when Staking_pseudotoken_repr.(frozen_deposits_pseudotokens <> zero) ->
        return (ctxt, frozen_deposits_pseudotokens)
    | _ ->
        let init_frozen_deposits_pseudotokens =
          Staking_pseudotoken_repr.of_int64_exn
            (Tez_repr.to_mutez frozen_deposits_tez)
        in
        let*! ctxt =
          Storage.Contract.Costaking_pseudotokens.add
            ctxt
            contract
            init_frozen_deposits_pseudotokens
        in
        return (ctxt, init_frozen_deposits_pseudotokens)
  in
  let*? new_frozen_deposits_pseudotokens, x =
    f ~frozen_deposits_pseudotokens ~frozen_deposits_tez
  in
  let*! ctxt =
    Storage.Contract.Frozen_deposits_pseudotokens.add
      ctxt
      contract
      new_frozen_deposits_pseudotokens
  in
  return (ctxt, x)

let credit_frozen_deposits_pseudotokens_for_tez_amount ctxt delegate tez_amount
    =
  if Tez_repr.(tez_amount = zero) then
    return (ctxt, Staking_pseudotoken_repr.zero)
  else
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

(** [debit_frozen_deposits_pseudotokens ctxt delegate p_amount] decreases
    [delegate]'s stake pseudotokens by [p_amount].
    The function also returns the amount of tez [p_amount] current worth.
*)
let debit_frozen_deposits_pseudotokens ctxt delegate pseudotoken_amount =
  if Staking_pseudotoken_repr.(pseudotoken_amount = zero) then
    return (ctxt, Tez_repr.zero)
  else
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

(** [costaking_pseudotokens_balance ctxt contract] returns [contract]'s
    current costaking balance. *)
let costaking_pseudotokens_balance ctxt contract =
  let open Lwt_result_syntax in
  let+ costaking_pseudotokens_opt =
    Storage.Contract.Costaking_pseudotokens.find ctxt contract
  in
  Option.value ~default:Staking_pseudotoken_repr.zero costaking_pseudotokens_opt

let costaking_balance_as_tez ctxt ~contract ~delegate =
  let open Lwt_result_syntax in
  let* pseudotokens = costaking_pseudotokens_balance ctxt contract in
  tez_of_frozen_deposits_pseudotokens ctxt delegate pseudotokens

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

(** [debit_costaking_pseudotokens ctxt contract p_amount] decreases
    [contract]'s costaking pseudotokens balance by [p_amount]. *)
let debit_costaking_pseudotokens ctxt contract pseudotokens_to_subtract =
  let f current_pseudotokens_balance =
    Staking_pseudotoken_repr.(
      current_pseudotokens_balance -? pseudotokens_to_subtract)
  in
  update_costaking_pseudotokens ~f ctxt contract

let request_unstake ctxt ~contract ~delegate requested_amount =
  let open Lwt_result_syntax in
  if Tez_repr.(requested_amount = zero) then return (ctxt, Tez_repr.zero)
  else
    let delegate_contract = Contract_repr.Implicit delegate in
    let* {current_amount = frozen_deposits_tez; initial_amount = _} =
      Frozen_deposits_storage.get ctxt delegate_contract
    in
    if Tez_repr.(frozen_deposits_tez = zero) then return (ctxt, Tez_repr.zero)
    else
      let* frozen_deposits_pseudotokens_opt =
        Storage.Contract.Frozen_deposits_pseudotokens.find
          ctxt
          delegate_contract
      in
      let* available_pseudotokens =
        costaking_pseudotokens_balance ctxt contract
      in
      match frozen_deposits_pseudotokens_opt with
      | Some frozen_deposits_pseudotokens
        when Staking_pseudotoken_repr.(frozen_deposits_pseudotokens <> zero) ->
          if Staking_pseudotoken_repr.(available_pseudotokens = zero) then
            return (ctxt, Tez_repr.zero)
          else
            let requested_pseudotokens =
              pseudotokens_of
                ~frozen_deposits_pseudotokens
                ~frozen_deposits_tez
                ~tez_amount:requested_amount
            in
            let pseudotokens_to_unstake =
              Staking_pseudotoken_repr.min
                requested_pseudotokens
                available_pseudotokens
            in
            assert (
              not Staking_pseudotoken_repr.(pseudotokens_to_unstake = zero)) ;
            let* ctxt, tez_to_unstake =
              debit_frozen_deposits_pseudotokens
                ctxt
                delegate
                pseudotokens_to_unstake
            in
            let+ ctxt =
              debit_costaking_pseudotokens ctxt contract pseudotokens_to_unstake
            in
            (ctxt, tez_to_unstake)
      | _ ->
          (* [delegate] must be non-costaked and have their pseudotokens
             non-initialized.
             Either the request is from a delegator with zero costake (hence
             nothing to unstake) or from the delegate themself and there is no
             need to initialize their pseudotokens. *)
          assert (Staking_pseudotoken_repr.(available_pseudotokens = zero)) ;
          if Contract_repr.(contract = delegate_contract) then
            return (ctxt, Tez_repr.min frozen_deposits_tez requested_amount)
          else return (ctxt, Tez_repr.zero)
