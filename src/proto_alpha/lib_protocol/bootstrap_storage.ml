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

open Misc

let init_account ctxt
    ({public_key_hash; public_key; amount} : Parameters_repr.bootstrap_account)
    =
  let contract = Contract_repr.implicit_contract public_key_hash in
  Contract_storage.credit ctxt contract amount >>=? fun ctxt ->
  match public_key with
  | Some public_key ->
      Contract_storage.reveal_manager_key ctxt public_key_hash public_key
      >>=? fun ctxt -> Delegate_storage.set ctxt contract (Some public_key_hash)
  | None -> return ctxt

let init_contract ~typecheck ctxt
    ({delegate; amount; script} : Parameters_repr.bootstrap_contract) =
  Contract_storage.fresh_contract_from_current_nonce ctxt
  >>?= fun (ctxt, contract) ->
  typecheck ctxt script >>=? fun (script, ctxt) ->
  Contract_storage.raw_originate
    ctxt
    contract
    ~balance:amount
    ~prepaid_bootstrap_storage:true
    ~script
    ~delegate:(Some delegate)

let init ctxt ~typecheck ?ramp_up_cycles ?no_reward_cycles accounts contracts =
  let nonce =
    Operation_hash.hash_bytes [Bytes.of_string "Un festival de GADT."]
  in
  let ctxt = Raw_context.init_origination_nonce ctxt nonce in
  List.fold_left_es init_account ctxt accounts >>=? fun ctxt ->
  List.fold_left_es (init_contract ~typecheck) ctxt contracts >>=? fun ctxt ->
  (match no_reward_cycles with
  | None -> return ctxt
  | Some cycles ->
      (* Store pending ramp ups. *)
      let constants = Raw_context.constants ctxt in
      (* Start without rewards *)
      Raw_context.patch_constants ctxt (fun c ->
          {
            c with
            baking_reward_per_endorsement = [Tez_repr.zero];
            endorsement_reward = [Tez_repr.zero];
          })
      >>= fun ctxt ->
      (* Store the final reward. *)
      Storage.Ramp_up.Rewards.init
        ctxt
        (Cycle_repr.of_int32_exn (Int32.of_int cycles))
        (constants.baking_reward_per_endorsement, constants.endorsement_reward))
  >>=? fun ctxt ->
  match ramp_up_cycles with
  | None -> return ctxt
  | Some cycles ->
      (* Store pending ramp ups. *)
      let constants = Raw_context.constants ctxt in
      Tez_repr.(constants.block_security_deposit /? Int64.of_int cycles)
      >>?= fun block_step ->
      Tez_repr.(constants.endorsement_security_deposit /? Int64.of_int cycles)
      >>?= fun endorsement_step ->
      (* Start without security_deposit *)
      Raw_context.patch_constants ctxt (fun c ->
          {
            c with
            block_security_deposit = Tez_repr.zero;
            endorsement_security_deposit = Tez_repr.zero;
          })
      >>= fun ctxt ->
      List.fold_left_es
        (fun ctxt cycle ->
          Tez_repr.(block_step *? Int64.of_int cycle)
          >>?= fun block_security_deposit ->
          Tez_repr.(endorsement_step *? Int64.of_int cycle)
          >>?= fun endorsement_security_deposit ->
          let cycle = Cycle_repr.of_int32_exn (Int32.of_int cycle) in
          Storage.Ramp_up.Security_deposits.init
            ctxt
            cycle
            (block_security_deposit, endorsement_security_deposit))
        ctxt
        (1 --> (cycles - 1))
      >>=? fun ctxt ->
      (* Store the final security deposits. *)
      Storage.Ramp_up.Security_deposits.init
        ctxt
        (Cycle_repr.of_int32_exn (Int32.of_int cycles))
        ( constants.block_security_deposit,
          constants.endorsement_security_deposit )

let cycle_end ctxt last_cycle =
  let next_cycle = Cycle_repr.succ last_cycle in
  (Storage.Ramp_up.Rewards.find ctxt next_cycle >>=? function
   | None -> return ctxt
   | Some (baking_reward_per_endorsement, endorsement_reward) ->
       Storage.Ramp_up.Rewards.remove_existing ctxt next_cycle >>=? fun ctxt ->
       Raw_context.patch_constants ctxt (fun c ->
           {c with baking_reward_per_endorsement; endorsement_reward})
       >|= ok)
  >>=? fun ctxt ->
  Storage.Ramp_up.Security_deposits.find ctxt next_cycle >>=? function
  | None -> return ctxt
  | Some (block_security_deposit, endorsement_security_deposit) ->
      Storage.Ramp_up.Security_deposits.remove_existing ctxt next_cycle
      >>=? fun ctxt ->
      Raw_context.patch_constants ctxt (fun c ->
          {c with block_security_deposit; endorsement_security_deposit})
      >|= ok
