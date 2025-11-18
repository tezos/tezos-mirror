(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type container =
  [ `Contract of Contract_repr.t
  | `Collected_commitments of Blinded_public_key_hash.t
  | `Frozen_deposits of Frozen_staker_repr.t
  | `Unstaked_frozen_deposits of Unstaked_frozen_staker_repr.t * Cycle_repr.t
  | `Block_fees
  | `Frozen_bonds of Contract_repr.t * Bond_id_repr.t ]

type infinite_source =
  [ `Invoice
  | `Bootstrap
  | `Initial_commitments
  | `Revelation_rewards
  | `Attesting_rewards
  | `Baking_rewards
  | `Baking_bonuses
  | `Dal_attesting_rewards
  | `Minted
  | `Liquidity_baking_subsidies
  | `Sc_rollup_refutation_rewards ]

type giver = [infinite_source | container]

type infinite_sink =
  [ `Storage_fees
  | `Double_signing_punishments
  | `Lost_attesting_rewards of Signature.Public_key_hash.t * bool * bool
  | `Lost_dal_attesting_rewards of Signature.Public_key_hash.t
  | `Sc_rollup_refutation_punishments
  | `Burned ]

type receiver = [infinite_sink | container]

let balance ctxt stored =
  let open Lwt_result_syntax in
  match stored with
  | `Collected_commitments bpkh ->
      let+ balance = Commitment_storage.committed_amount ctxt bpkh in
      (ctxt, balance)
  | `Block_fees -> return (ctxt, Raw_context.get_collected_fees ctxt)

let credit ctxt receiver amount origin =
  let open Lwt_result_syntax in
  let open Receipt_repr in
  let+ ctxt, balance =
    match receiver with
    | #infinite_sink as infinite_sink ->
        let sink =
          match infinite_sink with
          | `Storage_fees -> Storage_fees
          | `Double_signing_punishments -> Double_signing_punishments
          | `Lost_attesting_rewards (d, p, r) -> Lost_attesting_rewards (d, p, r)
          | `Lost_dal_attesting_rewards d -> Lost_dal_attesting_rewards d
          | `Sc_rollup_refutation_punishments ->
              Sc_rollup_refutation_punishments
          | `Burned -> Burned
        in
        let* old_total_supply = Storage.Contract.Total_supply.get ctxt in
        let*? new_total_supply = Tez_repr.(old_total_supply -? amount) in
        let+ ctxt =
          Storage.Contract.Total_supply.update ctxt new_total_supply
        in
        (ctxt, sink)
    | #container as container -> (
        match container with
        | `Contract receiver ->
            let+ ctxt =
              Contract_storage.credit_only_call_from_token ctxt receiver amount
            in
            (ctxt, Contract receiver)
        | `Collected_commitments bpkh ->
            let+ ctxt =
              Commitment_storage.increase_commitment_only_call_from_token
                ctxt
                bpkh
                amount
            in
            (ctxt, Commitments bpkh)
        | `Frozen_deposits staker ->
            let+ ctxt =
              Stake_storage.add_frozen_stake_only_call_from_token
                ctxt
                staker
                amount
            in
            (ctxt, Deposits staker)
        | `Unstaked_frozen_deposits (staker, cycle) ->
            let+ ctxt =
              Unstaked_frozen_deposits_storage.credit_only_call_from_token
                ctxt
                staker
                cycle
                amount
            in
            (ctxt, Unstaked_deposits (staker, cycle))
        | `Block_fees ->
            let*? ctxt =
              Raw_context.credit_collected_fees_only_call_from_token ctxt amount
            in
            return (ctxt, Block_fees)
        | `Frozen_bonds (contract, bond_id) ->
            let* ctxt =
              Contract_storage.credit_bond_only_call_from_token
                ctxt
                contract
                bond_id
                amount
            in
            return (ctxt, Frozen_bonds (contract, bond_id)))
  in
  (ctxt, item balance (Credited amount) origin)

let spend ctxt giver amount origin =
  let open Lwt_result_syntax in
  let open Receipt_repr in
  let+ ctxt, balance =
    match giver with
    | #infinite_source as infinite_source ->
        let src =
          match infinite_source with
          | `Bootstrap -> Bootstrap
          | `Invoice -> Invoice
          | `Initial_commitments -> Initial_commitments
          | `Minted -> Minted
          | `Liquidity_baking_subsidies -> Liquidity_baking_subsidies
          | `Revelation_rewards -> Nonce_revelation_rewards
          | `Attesting_rewards -> Attesting_rewards
          | `Baking_rewards -> Baking_rewards
          | `Baking_bonuses -> Baking_bonuses
          | `Dal_attesting_rewards -> Dal_attesting_rewards
          | `Sc_rollup_refutation_rewards -> Sc_rollup_refutation_rewards
        in
        let* old_total_supply = Storage.Contract.Total_supply.get ctxt in
        let*? new_total_supply = Tez_repr.(old_total_supply +? amount) in
        let+ ctxt =
          Storage.Contract.Total_supply.update ctxt new_total_supply
        in
        (ctxt, src)
    | #container as container -> (
        match container with
        | `Contract giver ->
            let+ ctxt =
              Contract_storage.spend_only_call_from_token ctxt giver amount
            in
            (ctxt, Contract giver)
        | `Collected_commitments bpkh ->
            let+ ctxt =
              Commitment_storage.decrease_commitment_only_call_from_token
                ctxt
                bpkh
                amount
            in
            (ctxt, Commitments bpkh)
        | `Frozen_deposits staker ->
            let+ ctxt =
              Stake_storage.remove_frozen_stake_only_call_from_token
                ctxt
                staker
                amount
            in
            (ctxt, Deposits staker)
        | `Unstaked_frozen_deposits (staker, cycle) ->
            let+ ctxt =
              Unstaked_frozen_deposits_storage.spend_only_call_from_token
                ctxt
                staker
                cycle
                amount
            in
            (ctxt, Unstaked_deposits (staker, cycle))
        | `Block_fees ->
            let*? ctxt =
              Raw_context.spend_collected_fees_only_call_from_token ctxt amount
            in
            return (ctxt, Block_fees)
        | `Frozen_bonds (contract, bond_id) ->
            let* ctxt =
              Contract_storage.spend_bond_only_call_from_token
                ctxt
                contract
                bond_id
                amount
            in
            return (ctxt, Frozen_bonds (contract, bond_id)))
  in
  (ctxt, item balance (Debited amount) origin)

let transfer_n ?(origin = Receipt_repr.Block_application) ctxt givers receiver =
  let open Lwt_result_syntax in
  let givers = List.filter (fun (_, am) -> Tez_repr.(am <> zero)) givers in
  match givers with
  | [] ->
      (* Avoid accessing context data when there is nothing to transfer. *)
      return (ctxt, [])
  | _ :: _ ->
      (* Withdraw from givers. *)
      let* ctxt, amount, debit_logs =
        List.fold_left_es
          (fun (ctxt, total, debit_logs) (giver, amount) ->
            let* ctxt, debit_log = spend ctxt giver amount origin in
            let*? total = Tez_repr.(amount +? total) in
            return (ctxt, total, debit_log :: debit_logs))
          (ctxt, Tez_repr.zero, [])
          givers
      in
      let* ctxt, credit_log = credit ctxt receiver amount origin in
      (* Deallocate implicit contracts with no stake. This must be done after
         spending and crediting. If done in between then a transfer of all the
         balance from (`Contract c) to (`Frozen_bonds (c,_)) would leave the
         contract c unallocated. *)
      let+ ctxt =
        List.fold_left_es
          (fun ctxt (giver, _amount) ->
            match giver with
            | `Contract contract | `Frozen_bonds (contract, _) ->
                Contract_storage.ensure_deallocated_if_empty ctxt contract
            | #giver -> return ctxt)
          ctxt
          givers
      in
      (* Make sure the order of balance updates is : debit logs in the order of
         of the parameter [givers], and then the credit log. *)
      let balance_updates = List.rev (credit_log :: debit_logs) in
      (ctxt, balance_updates)

let transfer ?(origin = Receipt_repr.Block_application) ctxt giver receiver
    amount =
  transfer_n ~origin ctxt [(giver, amount)] receiver

module Internal_for_tests = struct
  let allocated ctxt stored =
    let open Lwt_result_syntax in
    match stored with
    | `Contract contract ->
        let*! allocated = Contract_storage.allocated ctxt contract in
        return (ctxt, allocated)
    | `Collected_commitments bpkh ->
        let*! allocated = Commitment_storage.exists ctxt bpkh in
        return (ctxt, allocated)
    | `Frozen_deposits _ | `Unstaked_frozen_deposits _ | `Block_fees ->
        return (ctxt, true)
    | `Frozen_bonds (contract, bond_id) ->
        Contract_storage.bond_allocated ctxt contract bond_id

  type container_with_balance =
    [ `Contract of Contract_repr.t
    | `Collected_commitments of Blinded_public_key_hash.t
    | `Block_fees
    | `Frozen_bonds of Contract_repr.t * Bond_id_repr.t ]

  let balance ctxt (stored : [< container_with_balance]) =
    let open Lwt_result_syntax in
    match stored with
    | (`Collected_commitments _ | `Block_fees) as stored -> balance ctxt stored
    | `Contract contract ->
        let+ balance = Contract_storage.get_balance ctxt contract in
        (ctxt, balance)
    | `Frozen_bonds (contract, bond_id) ->
        let+ ctxt, balance_opt =
          Contract_storage.find_bond ctxt contract bond_id
        in
        (ctxt, Option.value ~default:Tez_repr.zero balance_opt)
end
