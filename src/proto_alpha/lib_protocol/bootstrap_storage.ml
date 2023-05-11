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

type error += Unrevealed_public_key of Signature.Public_key_hash.t

let () =
  register_error_kind
    `Permanent
    ~id:"bootstrap.unrevealed_public_key"
    ~title:"Forbidden delegation from unrevealed public key"
    ~description:"Tried to delegate from an unrevealed public key"
    ~pp:(fun ppf delegate ->
      Format.fprintf
        ppf
        "Delegation from an unrevealed public key (for %a) is forbidden."
        Signature.Public_key_hash.pp
        delegate)
    Data_encoding.(obj1 (req "delegator" Signature.Public_key_hash.encoding))
    (function Unrevealed_public_key pkh -> Some pkh | _ -> None)
    (fun pkh -> Unrevealed_public_key pkh)

let init_account (ctxt, balance_updates)
    ({public_key_hash; public_key; amount; delegate_to; consensus_key} :
      Parameters_repr.bootstrap_account) =
  let contract = Contract_repr.Implicit public_key_hash in
  Token.transfer
    ~origin:Protocol_migration
    ctxt
    `Bootstrap
    (`Contract contract)
    amount
  >>=? fun (ctxt, new_balance_updates) ->
  (match public_key with
  | Some public_key -> (
      Contract_manager_storage.reveal_manager_key
        ctxt
        public_key_hash
        public_key
      >>=? fun ctxt ->
      Delegate_storage.Contract.set
        ctxt
        contract
        (Some (Option.value ~default:public_key_hash delegate_to))
      >>=? fun ctxt ->
      match consensus_key with
      | None -> return ctxt
      | Some consensus_key ->
          Delegate_consensus_key.init ctxt public_key_hash consensus_key)
  | None ->
      fail_when
        (Option.is_some delegate_to)
        (Unrevealed_public_key public_key_hash)
      >>=? fun () -> return ctxt)
  >|=? fun ctxt -> (ctxt, new_balance_updates @ balance_updates)

let init_contract ~typecheck (ctxt, balance_updates)
    ({delegate; amount; script} : Parameters_repr.bootstrap_contract) =
  Contract_storage.fresh_contract_from_current_nonce ctxt
  >>?= fun (ctxt, contract_hash) ->
  typecheck ctxt script >>=? fun (script, ctxt) ->
  Contract_storage.raw_originate
    ctxt
    ~prepaid_bootstrap_storage:true
    contract_hash
    ~script
  >>=? fun ctxt ->
  let contract = Contract_repr.Originated contract_hash in
  (match delegate with
  | None -> return ctxt
  | Some delegate -> Delegate_storage.Contract.init ctxt contract delegate)
  >>=? fun ctxt ->
  let origin = Receipt_repr.Protocol_migration in
  Token.transfer ~origin ctxt `Bootstrap (`Contract contract) amount
  >|=? fun (ctxt, new_balance_updates) ->
  (ctxt, new_balance_updates @ balance_updates)

let init ctxt ~typecheck ?no_reward_cycles accounts contracts =
  let nonce = Operation_hash.hash_string ["Un festival de GADT."] in
  let ctxt = Raw_context.init_origination_nonce ctxt nonce in
  List.fold_left_es init_account (ctxt, []) accounts
  >>=? fun (ctxt, balance_updates) ->
  List.fold_left_es (init_contract ~typecheck) (ctxt, balance_updates) contracts
  >>=? fun (ctxt, balance_updates) ->
  (match no_reward_cycles with
  | None -> return ctxt
  | Some cycles ->
      (* Store pending ramp ups. *)
      let constants = Raw_context.constants ctxt in
      (* Start without rewards *)
      Raw_context.patch_constants ctxt (fun c ->
          {
            c with
            reward_weights =
              {
                c.reward_weights with
                base_total_rewards_per_minute = Tez_repr.zero;
              };
          })
      >>= fun ctxt ->
      (* Store the final reward. *)
      Storage.Ramp_up.(
        Rewards.init
          ctxt
          (Cycle_repr.of_int32_exn (Int32.of_int cycles))
          {
            (* Hack: we store the rewards here *)
            baking_reward_fixed_portion =
              constants.reward_weights.base_total_rewards_per_minute;
            baking_reward_bonus_per_slot = Tez_repr.zero;
            endorsing_reward_per_slot = Tez_repr.zero;
          }))
  >|=? fun ctxt -> (ctxt, balance_updates)

let cycle_end ctxt last_cycle =
  let next_cycle = Cycle_repr.succ last_cycle in
  Storage.Ramp_up.Rewards.find ctxt next_cycle >>=? function
  | None -> return ctxt
  | Some Storage.Ramp_up.{baking_reward_fixed_portion; _} ->
      Storage.Ramp_up.Rewards.remove_existing ctxt next_cycle >>=? fun ctxt ->
      Raw_context.patch_constants ctxt (fun c ->
          {
            c with
            reward_weights =
              {
                c.reward_weights with
                base_total_rewards_per_minute = baking_reward_fixed_portion;
              };
          })
      >|= ok
