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
  | `Delegate_balance of Signature.Public_key_hash.t
  | `Frozen_deposits of Signature.Public_key_hash.t
  | `Block_fees ]

type source =
  [ `Invoice
  | `Bootstrap
  | `Initial_commitments
  | `Revelation_rewards
  | `Double_signing_evidence_rewards
  | `Endorsing_rewards
  | `Baking_rewards
  | `Baking_bonuses
  | `Minted
  | `Liquidity_baking_subsidies
  | container ]

type sink =
  [ `Storage_fees
  | `Double_signing_punishments
  | `Lost_endorsing_rewards of Signature.Public_key_hash.t * bool * bool
  | `Burned
  | container ]

let allocated ctxt stored =
  match stored with
  | `Contract contract -> Contract_storage.allocated ctxt contract
  | `Collected_commitments bpkh -> Commitment_storage.exists ctxt bpkh >|= ok
  | `Delegate_balance delegate ->
      let contract = Contract_repr.implicit_contract delegate in
      Contract_storage.allocated ctxt contract
  | `Frozen_deposits delegate ->
      let contract = Contract_repr.implicit_contract delegate in
      Frozen_deposits_storage.allocated ctxt contract >|= ok
  | `Block_fees -> return_true

let balance ctxt stored =
  match stored with
  | `Contract contract -> Contract_storage.get_balance ctxt contract
  | `Collected_commitments bpkh -> Commitment_storage.committed_amount ctxt bpkh
  | `Delegate_balance delegate ->
      let contract = Contract_repr.implicit_contract delegate in
      Storage.Contract.Balance.get ctxt contract
  | `Frozen_deposits delegate -> (
      let contract = Contract_repr.implicit_contract delegate in
      Frozen_deposits_storage.find ctxt contract >|=? fun frozen_deposits ->
      match frozen_deposits with
      | None -> Tez_repr.zero
      | Some frozen_deposits -> frozen_deposits.current_amount)
  | `Block_fees -> return (Raw_context.get_collected_fees ctxt)

let credit ctxt dest amount origin =
  let open Receipt_repr in
  (match dest with
  | `Storage_fees -> return (ctxt, Storage_fees)
  | `Double_signing_punishments -> return (ctxt, Double_signing_punishments)
  | `Lost_endorsing_rewards (d, p, r) ->
      return (ctxt, Lost_endorsing_rewards (d, p, r))
  | `Burned -> return (ctxt, Burned)
  | `Contract dest ->
      Contract_storage.credit_only_call_from_token ctxt dest amount
      >|=? fun ctxt -> (ctxt, Contract dest)
  | `Collected_commitments bpkh ->
      Commitment_storage.increase_commitment_only_call_from_token
        ctxt
        bpkh
        amount
      >|=? fun ctxt -> (ctxt, Commitments bpkh)
  | `Delegate_balance delegate ->
      let contract = Contract_repr.implicit_contract delegate in
      Contract_storage.increase_balance_only_call_from_token
        ctxt
        contract
        amount
      >|=? fun ctxt -> (ctxt, Contract contract)
  | `Frozen_deposits delegate as dest ->
      allocated ctxt dest >>=? fun allocated ->
      (if not allocated then Frozen_deposits_storage.init ctxt delegate
      else return ctxt)
      >>=? fun ctxt ->
      Frozen_deposits_storage.credit_only_call_from_token ctxt delegate amount
      >|=? fun ctxt -> (ctxt, Deposits delegate)
  | `Block_fees ->
      Raw_context.credit_collected_fees_only_call_from_token ctxt amount
      >>?= fun ctxt -> return (ctxt, Block_fees))
  >|=? fun (ctxt, balance) -> (ctxt, (balance, Credited amount, origin))

let spend ctxt src amount origin =
  let open Receipt_repr in
  (match src with
  | `Bootstrap -> return (ctxt, Bootstrap)
  | `Invoice -> return (ctxt, Invoice)
  | `Initial_commitments -> return (ctxt, Initial_commitments)
  | `Minted -> return (ctxt, Minted)
  | `Liquidity_baking_subsidies -> return (ctxt, Liquidity_baking_subsidies)
  | `Revelation_rewards -> return (ctxt, Nonce_revelation_rewards)
  | `Double_signing_evidence_rewards ->
      return (ctxt, Double_signing_evidence_rewards)
  | `Endorsing_rewards -> return (ctxt, Endorsing_rewards)
  | `Baking_rewards -> return (ctxt, Baking_rewards)
  | `Baking_bonuses -> return (ctxt, Baking_bonuses)
  | `Contract src ->
      Contract_storage.spend_only_call_from_token ctxt src amount
      >|=? fun ctxt -> (ctxt, Contract src)
  | `Collected_commitments bpkh ->
      Commitment_storage.decrease_commitment_only_call_from_token
        ctxt
        bpkh
        amount
      >>=? fun ctxt -> return (ctxt, Commitments bpkh)
  | `Delegate_balance delegate ->
      let contract = Contract_repr.implicit_contract delegate in
      Contract_storage.decrease_balance_only_call_from_token
        ctxt
        contract
        amount
      >|=? fun ctxt -> (ctxt, Contract contract)
  | `Frozen_deposits delegate ->
      (if Tez_repr.(amount = zero) then return ctxt
      else
        Frozen_deposits_storage.spend_only_call_from_token ctxt delegate amount)
      >>=? fun ctxt -> return (ctxt, Deposits delegate)
  | `Block_fees ->
      Raw_context.spend_collected_fees_only_call_from_token ctxt amount
      >>?= fun ctxt -> return (ctxt, Block_fees))
  >|=? fun (ctxt, balance) -> (ctxt, (balance, Debited amount, origin))

let transfer_n ?(origin = Receipt_repr.Block_application) ctxt src dest =
  let sources = List.filter (fun (_, am) -> Tez_repr.(am <> zero)) src in
  match sources with
  | [] ->
      (* Avoid accessing context data when there is nothing to transfer. *)
      return (ctxt, [])
  | _ :: _ ->
      List.fold_left_es
        (fun (ctxt, total, debit_logs) (source, amount) ->
          spend ctxt source amount origin >>=? fun (ctxt, debit_log) ->
          Tez_repr.(amount +? total) >>?= fun total ->
          return (ctxt, total, debit_log :: debit_logs))
        (ctxt, Tez_repr.zero, [])
        sources
      >>=? fun (ctxt, amount, debit_logs) ->
      credit ctxt dest amount origin >|=? fun (ctxt, credit_log) ->
      (* Make sure the order of balance updates is : debit logs in the order of
         of the parameter [src], and then the credit log. *)
      let balance_updates = List.rev (credit_log :: debit_logs) in
      (ctxt, balance_updates)

let transfer ?(origin = Receipt_repr.Block_application) ctxt src dest amount =
  transfer_n ~origin ctxt [(src, amount)] dest
