(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2021 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(*
  To add invoices, you can use a helper function like this one:

  (** Invoice a contract at a given address with a given amount. Returns the
      updated context and a  balance update receipt (singleton list). The address
      must be a valid base58 hash, otherwise this is no-op and returns an empty
      receipts list.

      Do not fail if something goes wrong.
  *)
  let invoice_contract ctxt ~address ~amount_mutez =
    match Tez_repr.of_mutez amount_mutez with
    | None -> Lwt.return (ctxt, [])
    | Some amount -> (
        ( Contract_repr.of_b58check address >>?= fun recipient ->
          Token.transfer
            ~origin:Protocol_migration
            ctxt
            `Invoice
            (`Contract recipient)
            amount )
        >|= function
        | Ok res -> res
        | Error _ -> (ctxt, []))
*)

let unfreeze_deposits_rewards_and_fees ctxt delegate cycle =
  Token.balance ctxt (`Legacy_deposits (delegate, cycle)) >>=? fun deposits ->
  Token.balance ctxt (`Legacy_fees (delegate, cycle)) >>=? fun fees ->
  Token.balance ctxt (`Legacy_rewards (delegate, cycle)) >>=? fun rewards ->
  let contract = Contract_repr.implicit_contract delegate in
  Token.transfer_n
    ~origin:Protocol_migration
    ctxt
    [
      (`Legacy_deposits (delegate, cycle), deposits);
      (`Legacy_fees (delegate, cycle), fees);
      (`Legacy_rewards (delegate, cycle), rewards);
    ]
    (`Delegate_balance delegate)
  >>=? fun (ctxt, balance_updates) ->
  Contract_delegate_storage.add_contract_stake ctxt contract rewards
  >|=? fun ctxt -> (ctxt, balance_updates)

(* Note that Legacy_frozen_* tables do not need to be cleared because
   they become empty, given that when the amount in such a table
   becomes 0 the contract is automatically removed from the table (see
   the [Transfer] module). *)
let unfreeze_all_remaining_deposits_rewards_and_fees ctxt migration_cycle =
  let preserved = Constants_storage.preserved_cycles ctxt in
  List.fold_left_es
    (fun (ctxt, balance_updates) cycle_offset ->
      match Cycle_repr.sub migration_cycle cycle_offset with
      | None -> return (ctxt, balance_updates)
      | Some unfrozen_cycle ->
          Storage.Legacy_delegates_with_frozen_balance.fold
            (ctxt, unfrozen_cycle)
            ~order:`Sorted
            ~init:(Ok (ctxt, balance_updates))
            ~f:(fun delegate acc ->
              acc >>?= fun (ctxt, bus) ->
              unfreeze_deposits_rewards_and_fees ctxt delegate unfrozen_cycle
              >|=? fun (ctxt, balance_updates) -> (ctxt, balance_updates @ bus))
          >>=? fun (ctxt, balance_updates) ->
          Storage.Legacy_delegates_with_frozen_balance.clear
            (ctxt, unfrozen_cycle)
          >>= fun ctxt -> return (ctxt, balance_updates))
    (ctxt, [])
    Misc.(0 --> preserved)

let migrate_nonces ctxt migration_cycle =
  let migrate_cycle ctxt cycle =
    let levels = Level_storage.levels_with_commitments_in_cycle ctxt cycle in
    let migrate ctxt level =
      Storage.Seed.Nonce_legacy.mem ctxt level >>= function
      | false -> return ctxt
      | true ->
          Storage.Seed.Nonce_legacy.get ctxt level >>=? fun nonce ->
          Storage.Seed.Nonce.add ctxt level nonce >>= return
    in
    List.fold_left_es migrate ctxt levels
  in
  List.fold_left_es
    migrate_cycle
    ctxt
    (match Cycle_repr.pred migration_cycle with
    | None -> [migration_cycle]
    | Some previous_cycle -> [previous_cycle; migration_cycle])

let prepare_first_block ctxt ~typecheck ~level ~timestamp =
  Raw_context.prepare_first_block ~level ~timestamp ctxt
  >>=? fun (previous_protocol, ctxt) ->
  let cycle = (Raw_context.current_level ctxt).cycle in
  (match previous_protocol with
  | Genesis param ->
      (* This is the genesis protocol: initialise the state *)
      let init_commitment (ctxt, balance_updates)
          Commitment_repr.{blinded_public_key_hash; amount} =
        Token.transfer
          ctxt
          `Initial_commitments
          (`Collected_commitments blinded_public_key_hash)
          amount
        >>=? fun (ctxt, new_balance_updates) ->
        return (ctxt, new_balance_updates @ balance_updates)
      in
      List.fold_left_es init_commitment (ctxt, []) param.commitments
      >>=? fun (ctxt, commitments_balance_updates) ->
      Storage.Stake.Last_snapshot.init ctxt 0 >>=? fun ctxt ->
      Seed_storage.init ?initial_seed:param.constants.initial_seed ctxt
      >>=? fun ctxt ->
      Contract_storage.init ctxt >>=? fun ctxt ->
      Bootstrap_storage.init
        ctxt
        ~typecheck
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
      >>=? fun (ctxt, bootstrap_balance_updates) ->
      Stake_storage.init_first_cycles ctxt Delegate_storage.pubkey
      >>=? fun ctxt ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Storage.Block_round.init ctxt Round_repr.zero >>=? fun ctxt ->
      Vote_storage.update_listings ctxt >>=? fun ctxt ->
      (* Must be called after other originations since it unsets the origination nonce.*)
      Liquidity_baking_migration.init ctxt ~typecheck
      >>=? fun (ctxt, operation_results) ->
      Storage.Pending_migration.Operation_results.init ctxt operation_results
      >>=? fun ctxt ->
      Raw_level_repr.of_int32 level >>?= fun first_level ->
      Storage.Tenderbake.First_level.init ctxt first_level >>=? fun ctxt ->
      return (ctxt, commitments_balance_updates @ bootstrap_balance_updates)
  | Hangzhou_011 ->
      Raw_level_repr.of_int32 level >>?= fun first_level ->
      Storage.Tenderbake.First_level.init ctxt first_level >>=? fun ctxt ->
      Storage.Block_round.init ctxt Round_repr.zero >>=? fun ctxt ->
      Raw_context.remove_existing_tree ctxt ["block_priority"] >>=? fun ctxt ->
      Storage.Legacy_active_delegates_with_rolls.fold
        ctxt
        ~order:`Sorted
        ~init:(Ok ctxt)
        ~f:(fun pkh ctxt ->
          ctxt >>?= fun ctxt ->
          Storage.Roll_legacy.Delegate_roll_list.remove_existing ctxt pkh)
      >>=? fun ctxt ->
      Storage.Legacy_active_delegates_with_rolls.clear ctxt >>= fun ctxt ->
      let old_tokens_per_roll = Constants_storage.tokens_per_roll ctxt in
      let new_tokens_per_roll = Tez_repr.(mul_exn one 6_000) in
      assert (Tez_repr.(new_tokens_per_roll < old_tokens_per_roll)) ;
      Roll_storage_legacy.fold
        ctxt
        ~f:(fun _roll pk (stakes, pk_map) ->
          let (pkh, pk_map) =
            match Misc.Public_key_map.find pk pk_map with
            | None ->
                let pkh = Signature.Public_key.hash pk in
                (pkh, Misc.Public_key_map.add pk pkh pk_map)
            | Some pkh -> (pkh, pk_map)
          in
          let stake =
            Signature.Public_key_hash.Map.update
              pkh
              (function None -> Some 1l | Some n -> Some (Int32.succ n))
              stakes
          in
          return (stake, pk_map))
        (Signature.Public_key_hash.Map.empty, Misc.Public_key_map.empty)
      >>=? fun (stakes, _pk_map) ->
      Storage.Delegates.fold
        ctxt
        ~order:`Sorted
        ~init:(Ok ctxt)
        ~f:(fun pkh ctxt ->
          ctxt >>?= fun ctxt ->
          Roll_storage_legacy.get_change ctxt pkh >>=? fun change ->
          Storage.Roll_legacy.Delegate_change.remove ctxt pkh >>= fun ctxt ->
          Frozen_deposits_storage.init ctxt pkh >>=? fun ctxt ->
          Delegate_activation_storage.is_inactive ctxt pkh >>=? fun inactive ->
          match Signature.Public_key_hash.Map.find pkh stakes with
          | None ->
              Storage.Stake.Staking_balance.init ctxt pkh change
              >>=? fun ctxt ->
              if (not inactive) && Tez_repr.(change >= new_tokens_per_roll) then
                Storage.Stake.Active_delegate_with_one_roll.add ctxt pkh ()
                >>= fun ctxt -> return ctxt
              else return ctxt
          | Some n ->
              Lwt.return
                ( Tez_repr.(old_tokens_per_roll *? Int64.of_int32 n)
                >>? fun rolls -> Tez_repr.(rolls +? change) )
              >>=? fun staking_balance ->
              Storage.Stake.Staking_balance.init ctxt pkh staking_balance
              >>=? fun ctxt ->
              (if not inactive then
               Storage.Stake.Active_delegate_with_one_roll.add ctxt pkh ()
              else Lwt.return ctxt)
              >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      Raw_context.patch_constants ctxt (fun constants ->
          {
            constants with
            Constants_repr.tokens_per_roll = Tez_repr.(mul_exn one 6_000);
          })
      >>= fun ctxt ->
      (* NOTE: the code below fails when the migration happens during
         the first cycle after Genesis, probably because of a bug in
         the initialization of the previous protocol from Genesis. *)
      let preserved = Constants_storage.preserved_cycles ctxt in
      let max_slashing_period = Constants_storage.max_slashing_period ctxt in
      List.fold_left_s
        (fun ctxt cycle ->
          Storage.Roll_legacy.Last_for_snapshot.clear (ctxt, cycle)
          >>= fun ctxt ->
          Storage.Roll_legacy.Snapshot_for_cycle.remove ctxt cycle)
        ctxt
        Cycle_repr.(
          (match Cycle_repr.sub cycle preserved with
          | None -> cycle
          | Some cycle -> cycle)
          ---> Cycle_repr.add cycle (preserved + 3))
      >>= fun ctxt ->
      Raw_context.remove_existing_tree ctxt ["rolls"] >>=? fun ctxt ->
      migrate_nonces ctxt cycle >>=? fun ctxt ->
      unfreeze_all_remaining_deposits_rewards_and_fees ctxt cycle
      >>=? fun (ctxt, balance_updates) ->
      Storage.Stake.Last_snapshot.init ctxt 0 >>=? fun ctxt ->
      List.fold_left_es
        (fun ctxt cycle ->
          Storage.Seed.For_cycle.mem ctxt cycle >>= function
          | false -> return ctxt
          | true ->
              Stake_storage.snapshot ctxt >>=? fun ctxt ->
              Stake_storage
              .select_distribution_for_cycle_do_not_call_except_for_migration
                ctxt
                cycle
                Delegate_storage.pubkey)
        ctxt
        Cycle_repr.(
          (match Cycle_repr.sub cycle (max_slashing_period - 1) with
          | None -> cycle
          | Some cycle -> cycle)
          ---> Cycle_repr.add cycle (preserved + 1))
      >>=? fun ctxt ->
      (* Remove seeds that will not be useful any longer: those from
         [cycle - preserved_cycles - 1] to [cycle -  max_slashable_period].
         NB: The seed at [cycle - preserved] is already removed by
         H.cycle_end.  The seed at [cycle - max_slashable_period + 1]
         is removed a bit later below by [Stake_storage.clear_at_cycle_end]. *)
      (match
         ( Cycle_repr.sub cycle (preserved - 1),
           Cycle_repr.sub cycle max_slashing_period )
       with
      | (Some from_cycle, Some to_cycle) ->
          List.fold_left_es
            (fun ctxt cycle ->
              Storage.Seed.For_cycle.mem ctxt cycle >>= function
              | false -> return ctxt
              | true -> Storage.Seed.For_cycle.remove_existing ctxt cycle)
            ctxt
            Cycle_repr.(from_cycle ---> to_cycle)
      | _ -> return ctxt)
      >>=? fun ctxt -> return (ctxt, balance_updates))
  >>=? fun (ctxt, balance_updates) ->
  Stake_storage.snapshot ctxt >>=? fun ctxt ->
  Delegate_storage.freeze_deposits_do_not_call_except_for_migration
    ~new_cycle:cycle
    ~balance_updates
    ctxt
  >>=? fun (ctxt, balance_updates) ->
  (match Level_storage.dawn_of_a_new_cycle ctxt with
  | None -> return ctxt
  | Some last_cycle ->
      assert (Cycle_repr.(last_cycle = cycle)) ;
      Stake_storage.clear_at_cycle_end ctxt ~new_cycle:(Cycle_repr.succ cycle))
  >>=? fun ctxt ->
  Receipt_repr.group_balance_updates balance_updates >>?= fun balance_updates ->
  Storage.Pending_migration.Balance_updates.add ctxt balance_updates
  >>= fun ctxt -> return ctxt

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  >>=? fun ctxt -> Storage.Pending_migration.remove ctxt
