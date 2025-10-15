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

type error += Delegator_with_consensus_key of Signature.Public_key_hash.t

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
    (fun pkh -> Unrevealed_public_key pkh) ;
  register_error_kind
    `Permanent
    ~id:"bootstrap.delegator_with_consensus_key"
    ~title:"Forbidden delegation from bootstrap with consensus key"
    ~description:"Tried to set a consensus key to a non delegate"
    ~pp:(fun ppf delegate ->
      Format.fprintf
        ppf
        "Setting both a consensus key and a delegate other than self (for %a) \
         is forbidden."
        Signature.Public_key_hash.pp
        delegate)
    Data_encoding.(obj1 (req "bootstrap" Signature.Public_key_hash.encoding))
    (function Delegator_with_consensus_key pkh -> Some pkh | _ -> None)
    (fun pkh -> Delegator_with_consensus_key pkh)

let init_account (ctxt, balance_updates)
    ({public_key_hash; public_key; amount; delegate_to; consensus_key} :
      Parameters_repr.bootstrap_account) =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit public_key_hash in
  let* ctxt, new_balance_updates =
    Token.transfer
      ~origin:Protocol_migration
      ctxt
      `Bootstrap
      (`Contract contract)
      amount
  in
  let+ ctxt, freeze_balance_updates =
    match public_key with
    | Some public_key -> (
        let* ctxt =
          Contract_manager_storage.reveal_manager_key
            ctxt
            public_key_hash
            public_key
        in
        let* ctxt =
          match (consensus_key, delegate_to) with
          | None, None ->
              Delegate_storage.Contract.set ctxt contract (Some public_key_hash)
          | None, Some delegate_to ->
              Delegate_storage.Contract.set ctxt contract (Some delegate_to)
          | Some consensus_key, None ->
              let* ctxt =
                Delegate_storage.Contract.set
                  ctxt
                  contract
                  (Some public_key_hash)
              in
              Delegate_consensus_key.init_bootstrap
                ctxt
                public_key_hash
                consensus_key
          | Some _, Some _ ->
              tzfail (Delegator_with_consensus_key public_key_hash)
        in
        match delegate_to with
        | Some delegate
          when Signature.Public_key_hash.(delegate <> public_key_hash) ->
            return (ctxt, [])
        | _ ->
            (* Self-delegated => contract is a delegate.
               Freeze the largest amount of tokens to avoid over-delegation
               according to the [limit_of_delegation_over_baking].
               This is necessary so that the network (in tests too) starts with
               accounts with baking rights. *)
            let limit_of_delegation_over_baking =
              Constants_storage.limit_of_delegation_over_baking ctxt
            in
            let amount_to_freeze =
              let minimal_to_bake =
                let minimal_stake = Constants_storage.minimal_stake ctxt in
                let minimal_frozen_stake =
                  Constants_storage.minimal_frozen_stake ctxt
                in
                Tez_repr.max minimal_stake minimal_frozen_stake
              in
              let minimal_to_not_be_overdelegated =
                Tez_repr.div_exn amount (limit_of_delegation_over_baking + 1)
              in
              Tez_repr.(
                min amount (max minimal_to_bake minimal_to_not_be_overdelegated))
            in
            Token.transfer
              ~origin:Protocol_migration
              ctxt
              (`Contract contract)
              (`Frozen_deposits (Frozen_staker_repr.baker public_key_hash))
              amount_to_freeze)
    | None ->
        let* () =
          fail_when
            (Option.is_some delegate_to)
            (Unrevealed_public_key public_key_hash)
        in
        return (ctxt, [])
  in
  (ctxt, freeze_balance_updates @ new_balance_updates @ balance_updates)

let init_contract ~typecheck_smart_contract (ctxt, balance_updates)
    ({delegate; amount; script; hash} : Parameters_repr.bootstrap_contract) =
  let open Lwt_result_syntax in
  let*? ctxt, contract_hash =
    match hash with
    | None -> Contract_storage.fresh_contract_from_current_nonce ctxt
    | Some hash -> Result.return (ctxt, hash)
  in
  let* script, ctxt = typecheck_smart_contract ctxt script in
  let* ctxt =
    Contract_storage.raw_originate
      ctxt
      ~prepaid_bootstrap_storage:true
      contract_hash
      ~script
  in
  let contract = Contract_repr.Originated contract_hash in
  let* ctxt =
    match delegate with
    | None -> return ctxt
    | Some delegate -> Delegate_storage.Contract.init ctxt contract delegate
  in
  let origin = Receipt_repr.Protocol_migration in
  let+ ctxt, new_balance_updates =
    Token.transfer ~origin ctxt `Bootstrap (`Contract contract) amount
  in
  (ctxt, new_balance_updates @ balance_updates)

let init_smart_rollup ~typecheck_smart_rollup ctxt
    ({address; boot_sector; pvm_kind; parameters_ty; whitelist} :
      Parameters_repr.bootstrap_smart_rollup) =
  let open Lwt_result_syntax in
  let*? ctxt =
    let open Result_syntax in
    let* parameters_ty = Script_repr.force_decode parameters_ty in
    typecheck_smart_rollup ctxt parameters_ty
  in
  let*! genesis_hash = Sc_rollups.genesis_state_hash_of pvm_kind ~boot_sector in
  let genesis_commitment : Sc_rollup_commitment_repr.t =
    {
      compressed_state = genesis_hash;
      (* Level 0: Genesis block.
         Level 1: Block on protocol genesis, that only activates protocols.
         Level 2: First block on the activated protocol.

         Therefore we originate the rollup at level 2 so the rollup node
         doesn't ask a block on a different protocol.
      *)
      inbox_level = Raw_level_repr.of_int32_exn 2l;
      predecessor = Sc_rollup_commitment_repr.Hash.zero;
      number_of_ticks = Sc_rollup_repr.Number_of_ticks.zero;
    }
  in
  let* _, _, ctxt =
    Sc_rollup_storage.raw_originate
      ctxt
      ~kind:pvm_kind
      ~genesis_commitment
      ~parameters_ty
      ~address
      ?whitelist
  in
  return ctxt

let init ctxt ~typecheck_smart_contract ~typecheck_smart_rollup
    ?no_reward_cycles accounts contracts smart_rollups =
  let open Lwt_result_syntax in
  let nonce = Operation_hash.hash_string ["Un festival de GADT."] in
  let ctxt = Raw_context.init_origination_nonce ctxt nonce in
  let* ctxt, balance_updates =
    List.fold_left_es init_account (ctxt, []) accounts
  in
  let* ctxt, balance_updates =
    List.fold_left_es
      (init_contract ~typecheck_smart_contract)
      (ctxt, balance_updates)
      contracts
  in
  let* ctxt =
    List.fold_left_es
      (init_smart_rollup ~typecheck_smart_rollup)
      ctxt
      smart_rollups
  in
  let+ ctxt =
    match no_reward_cycles with
    | None -> return ctxt
    | Some cycles ->
        (* Store pending ramp ups. *)
        let constants = Raw_context.constants ctxt in
        (* Start without rewards *)
        let*! ctxt =
          Raw_context.patch_constants ctxt (fun c ->
              {
                c with
                issuance_weights =
                  {
                    c.issuance_weights with
                    base_total_issued_per_minute = Tez_repr.zero;
                  };
              })
        in
        (* Store the final reward. *)
        Storage.Ramp_up.(
          Rewards.init
            ctxt
            (Cycle_repr.of_int32_exn (Int32.of_int cycles))
            {
              (* Hack: we store the rewards here *)
              baking_reward_fixed_portion =
                constants.issuance_weights.base_total_issued_per_minute;
              baking_reward_bonus_per_slot = Tez_repr.zero;
              attesting_reward_per_slot = Tez_repr.zero;
              dal_attesting_reward_per_shard = Tez_repr.zero;
            })
  in
  (ctxt, balance_updates)

let cycle_end ctxt last_cycle =
  let open Lwt_result_syntax in
  let next_cycle = Cycle_repr.succ last_cycle in
  let* result = Storage.Ramp_up.Rewards.find ctxt next_cycle in
  match result with
  | None -> return ctxt
  | Some Storage.Ramp_up.{baking_reward_fixed_portion; _} ->
      let* ctxt = Storage.Ramp_up.Rewards.remove_existing ctxt next_cycle in
      let*! ctxt =
        Raw_context.patch_constants ctxt (fun c ->
            {
              c with
              issuance_weights =
                {
                  c.issuance_weights with
                  base_total_issued_per_minute = baking_reward_fixed_portion;
                };
            })
      in
      return ctxt
