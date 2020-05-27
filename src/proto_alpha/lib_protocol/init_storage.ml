(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Logging

let migrate_baker :
    Raw_context.t ->
    Signature.Public_key_hash.t ->
    Raw_context.t tzresult Lwt.t =
 fun ctxt baker_pkh ->
  lwt_log_notice
    "\nMigrating baker %a..."
    Signature.Public_key_hash.pp
    baker_pkh
  >>= fun () ->
  Contract_storage.get_public_key
    ctxt
    (Contract_repr.implicit_contract baker_pkh)
  >>=? fun pk ->
  lwt_log_notice "pk %a" Signature.Public_key.pp pk
  >>= fun () ->
  let storage = Baker_script_repr.storage ~threshold:1 ~owner_keys:[pk] in
  Baker_storage.fresh_baker_from_current_nonce ctxt
  >>=? fun (ctxt, baker_hash) ->
  lwt_log_notice "baker_hash %a" Baker_hash.pp baker_hash
  >>= fun () ->
  let from = baker_pkh in
  let to_ = baker_hash in
  let from_contract = Contract_repr.implicit_contract from in
  let to_contract = Contract_repr.baker_contract to_ in
  Storage.Baker.Registered.add ctxt baker_hash
  >>= fun ctxt ->
  Storage.Contract.Storage.init
    ctxt
    to_contract
    (Script_repr.lazy_expr storage)
  >>=? fun (ctxt, storage_size) ->
  let total_size = Z.of_int storage_size in
  Storage.Contract.Paid_storage_space.init ctxt to_contract total_size
  >>=? fun ctxt ->
  Storage.Contract.Used_storage_space.init ctxt to_contract total_size
  >>=? fun ctxt ->
  Storage.Contract.Global_counter.get ctxt
  >>=? fun counter ->
  Storage.Contract.Counter.init ctxt to_contract counter
  >>=? fun ctxt ->
  Storage.Baker.Consensus_key.init ctxt baker_hash pk
  >>=? fun ctxt ->
  let pkh = Signature.Public_key.hash pk in
  Storage.Baker.Consensus_key_rev.init ctxt pkh baker_hash
  >>=? fun ctxt ->
  Storage.Delegates_008.remove ctxt baker_pkh
  >>= fun ctxt ->
  (* Migrate storages with original baker pkh as a key *)
  Storage.Contract.Balance.get ctxt from_contract
  >>=? fun balance ->
  lwt_log_notice "moving balance %a" Tez_repr.pp balance
  >>= fun () ->
  Storage.Contract.Balance.init ctxt to_contract balance
  >>=? fun ctxt ->
  Storage.Contract.Balance.remove ctxt from_contract
  >>= fun ctxt ->
  (* Roll.Delegate_roll_list -> Roll.Baker_roll_list *)
  Storage.Roll.Delegate_roll_list_008.find ctxt from
  >>=? (function
         | None ->
             lwt_log_notice "no rolls in the list" >>= fun () -> return ctxt
         | Some roll ->
             Storage.Roll.Delegate_roll_list_008.remove ctxt from
             >>= fun ctxt -> Storage.Roll.Baker_roll_list.init ctxt to_ roll)
  >>=? fun ctxt ->
  (* Roll.Delegate_change -> Roll.Baker_change *)
  Storage.Roll.Delegate_change_008.find ctxt from
  >>=? (function
         | None ->
             lwt_log_notice "no change" >>= fun () -> return ctxt
         | Some change ->
             Storage.Roll.Delegate_change_008.remove ctxt from
             >>= fun ctxt -> Storage.Roll.Baker_change.init ctxt to_ change)
  >>=? fun ctxt ->
  (* Contract.Delegated -> Baker.Delegators *)
  Storage.Contract.Delegated_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun contract acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      ( match Contract_repr.is_implicit contract with
      | None ->
          return_some contract
      | Some pkh ->
          (* if it's self-delegation, remove the association *)
          if Signature.Public_key_hash.equal pkh from then return_none
          else return_some contract )
      >>=? function
      | Some contract ->
          Storage.Baker.Delegators.add (ctxt, to_) contract
          >>= fun ctxt -> return ctxt
      | None ->
          return ctxt)
  >>=? fun ctxt ->
  Storage.Contract.Delegated_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_deposits -> Baker.Frozen_deposits *)
  Storage.Contract.Frozen_deposits_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle deposit acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      lwt_log_notice
        "moving frozen deposits %a for cycle %a"
        Tez_repr.pp
        deposit
        Cycle_repr.pp
        cycle
      >>= fun () ->
      Storage.Baker.Frozen_deposits.init (ctxt, to_) cycle deposit)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_deposits_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_fees -> Baker.Frozen_fees *)
  Storage.Contract.Frozen_fees_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle fee acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      lwt_log_notice
        " moving frozen fees %a for cycle %a"
        Tez_repr.pp
        fee
        Cycle_repr.pp
        cycle
      >>= fun () -> Storage.Baker.Frozen_fees.init (ctxt, to_) cycle fee)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_fees_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_rewards -> Baker.Frozen_rewards *)
  Storage.Contract.Frozen_rewards_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle rewards acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      lwt_log_notice
        "moving frozen rewards %a for cycle %a"
        Tez_repr.pp
        rewards
        Cycle_repr.pp
        cycle
      >>= fun () -> Storage.Baker.Frozen_rewards.init (ctxt, to_) cycle rewards)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_rewards_008.clear (ctxt, from_contract)
  >>= fun ctxt -> return ctxt

(* This is the genesis protocol: initialise the state *)
let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block ~level ~timestamp ~fitness ctxt
  >>=? fun (previous_protocol, ctxt) ->
  match previous_protocol with
  | Genesis param ->
      Commitment_storage.init ctxt param.commitments
      >>=? fun ctxt ->
      Roll_storage.init ctxt
      >>=? fun ctxt ->
      Seed_storage.init ctxt
      >>=? fun ctxt ->
      Contract_storage.init ctxt
      >>=? fun ctxt ->
      Bootstrap_storage.init
        ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
        param.bootstrap_bakers
      >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Baker_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0
      >>=? fun ctxt -> Vote_storage.update_listings ctxt
  | Edo_008 ->
      (* 1. Baker accounts migration *)
      let nonce =
        Operation_hash.hash_bytes
          [ Bytes.of_string
              "Things will not calm down, Daniel Jackson. They will, in fact, \
               calm up." ]
      in
      let ctxt = Raw_context.init_origination_nonce ctxt nonce in
      Storage.Delegates_008.fold ctxt ~init:(ok ctxt) ~f:(fun baker_pkh acc ->
          Lwt.return acc >>=? fun ctxt -> migrate_baker ctxt baker_pkh)
      >>=? fun ctxt ->
      Storage.Delegates_008.clear ctxt
      >>= fun ctxt ->
      let current_cycle = Raw_context.(current_level ctxt).cycle in
      (* Take a snapshot of the consensus keys *)
      lwt_log_notice
        "snapshot consensus keys in cycle %a"
        Cycle_repr.pp
        current_cycle
      >>= fun () ->
      Storage.Baker.Consensus_key.snapshot ctxt current_cycle
      >>=? fun ctxt ->
      (* Because the migration runs on the last block of a predecessor protocol,
         we also need to take the snapshot for the following cycle, otherwise
         the baker process which looks-up consensus for the future block would
         fail. *)
      Storage.Baker.Consensus_key.snapshot ctxt (Cycle_repr.succ current_cycle)
      >>=? fun ctxt ->
      (* Migrate storages with references to original baker pkhs *)
      (* Contract.Delegate *)
      Storage.Contract.Delegate_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun contract pkh acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.find ctxt pkh
          >>=? function
          | None ->
              (* NOTE There's one contract
                 "tz1gNQpjio7F5HsR9bs37vBYZXfbWpZ34tpL" that's not registered
                 as baker, but because of a bug in protocol 001 it has two
                 originated contracts delegated to it:
                  - KT1LLKuQLmxciFUif4yt6UyXFqEE3235emHE
                  - KT19MYkajshb4DnpDyNC37Qm7DCbPh5GP5Sg
                 We drop the delegations here.
              *)
              Storage.Contract.Delegate_008.remove ctxt contract >|= ok
          | Some baker_hash -> (
              let migrate_delegate ctxt =
                (* Because the path has not changed, this will override 008
                   value *)
                Storage.Contract.Delegate.update ctxt contract baker_hash
              in
              match Contract_repr.is_implicit contract with
              | Some contract_pkh -> (
                  (* Check this the contract is a baker *)
                  Storage.Baker.Consensus_key_rev.find ctxt contract_pkh
                  >>=? function
                  | None ->
                      migrate_delegate ctxt
                  | Some _ ->
                      (* Baker contracts are no longer self-delegated, so we
                         don't migrate this relation, instead we delete it *)
                      Storage.Contract.Delegate_008.remove ctxt contract >|= ok
                  )
              | None ->
                  migrate_delegate ctxt ))
      >>=? fun ctxt ->
      (* Active_delegates_with_rolls -> Baker.Active_with_rolls *)
      Storage.Active_delegates_with_rolls_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Baker.Active_with_rolls.add ctxt baker
          >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      Storage.Active_delegates_with_rolls_008.clear ctxt
      >>= fun ctxt ->
      let preserved = Constants_storage.preserved_cycles ctxt in
      (* Delegates_with_frozen_balance -> Baker.With_frozen_balance *)
      let rec migrate_frozen_balance ctxt cycle =
        lwt_log_notice
          "migrating bakers with frozen balance for cycle %a"
          Cycle_repr.pp
          cycle
        >>= fun () ->
        Storage.Delegates_with_frozen_balance_008.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun pkh acc ->
            Lwt.return acc
            >>=? fun ctxt ->
            Storage.Baker.Consensus_key_rev.get ctxt pkh
            >>=? fun baker ->
            Storage.Baker.With_frozen_balance.add (ctxt, cycle) baker
            >>= fun ctxt -> return ctxt)
        >>=? fun ctxt ->
        Storage.Delegates_with_frozen_balance_008.clear (ctxt, cycle)
        >>= fun ctxt ->
        match Cycle_repr.pred cycle with
        | None ->
            return ctxt
        | Some prev_cycle ->
            if Cycle_repr.(current_cycle = add prev_cycle (preserved + 1)) then
              return ctxt
            else migrate_frozen_balance ctxt prev_cycle
      in
      migrate_frozen_balance ctxt current_cycle
      >>=? fun ctxt ->
      (* Contract.Inactive_delegate -> Baker.Inactive *)
      Storage.Contract.Inactive_delegate_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun inactive_baker acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          match Contract_repr.is_implicit inactive_baker with
          | None ->
              let message =
                Format.asprintf
                  "inactive baker `Contract.Inactive_delegate_008` \"%a\" is \
                   not implicit account"
                  Contract_repr.pp
                  inactive_baker
              in
              failwith message
          | Some baker ->
              Storage.Baker.Consensus_key_rev.get ctxt baker
              >>=? fun migrated_baker ->
              Storage.Baker.Inactive.add ctxt migrated_baker
              >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      Storage.Contract.Inactive_delegate_008.clear ctxt
      >>= fun ctxt ->
      (* Contract.Delegate_desactivation -> Baker.Inactive *)
      Storage.Contract.Delegate_desactivation_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun baker cycle acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          match Contract_repr.is_implicit baker with
          | None ->
              let message =
                Format.asprintf
                  "inactive baker `Contract.Delegate_desactivation_008` \
                   \"%a\" is not implicit account"
                  Contract_repr.pp
                  baker
              in
              failwith message
          | Some baker ->
              Storage.Baker.Consensus_key_rev.get ctxt baker
              >>=? fun migrated_baker ->
              Storage.Baker.Deactivation.init ctxt migrated_baker cycle)
      >>=? fun ctxt ->
      Storage.Contract.Delegate_desactivation_008.clear ctxt
      >>= fun ctxt ->
      (* Roll.Owner *)
      Storage.Roll.Owner_008.fold ctxt ~init:(ok ctxt) ~f:(fun roll pk acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          let pkh = Signature.Public_key.hash pk in
          lwt_log_notice "migrating roll %ld owner" (Roll_repr.to_int32 roll)
          >>= fun () ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          (* because the path has not changed, this will override 008 value *)
          Storage.Roll.Owner.update ctxt roll baker)
      >>=? fun ctxt ->
      Storage.Roll.Owner_008.Snapshot.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun key pk acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          let pkh = Signature.Public_key.hash pk in
          lwt_log_notice
            "migrating roll %ld owner snapshot for cycle %a"
            (Roll_repr.to_int32 (snd key))
            Cycle_repr.pp
            (fst (fst key))
          >>= fun () ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          (* because the path has not changed, this will override 008 value *)
          Storage.Roll.Owner.Snapshot.update ctxt key baker)
      >>=? fun ctxt ->
      (* Vote.Listings *)
      Storage.Vote.Listings_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh listings acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Listings_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Listings.init ctxt baker listings)
      >>=? fun ctxt ->
      (* Vote.Proposals *)
      Storage.Vote.Proposals_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun (proposal, pkh) acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Proposals_008.remove ctxt (proposal, pkh)
          >>= fun ctxt ->
          Storage.Vote.Proposals.add ctxt (proposal, baker)
          >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      (* Vote.Proposals_count *)
      Storage.Vote.Proposals_count_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh count acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Proposals_count_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Proposals_count.init ctxt baker count)
      >>=? fun ctxt ->
      (* Vote.Ballots *)
      Storage.Vote.Ballots_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh ballot acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Storage.Baker.Consensus_key_rev.get ctxt pkh
          >>=? fun baker ->
          Storage.Vote.Ballots_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Ballots.init ctxt baker ballot)
      >>=? fun ctxt ->
      (* Storage.Seed.Nonce *)
      let rec migrate_cycle_nonce ctxt cycle =
        Storage.Cycle_008.Nonce.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun level nonce acc ->
            Lwt.return acc
            >>=? fun ctxt ->
            ( match nonce with
            | Unrevealed {nonce_hash; delegate; rewards; fees} ->
                lwt_log_notice
                  "migrating unrevealed nonce %a"
                  Nonce_hash.pp
                  nonce_hash
                >>= fun () ->
                Storage.Baker.Consensus_key_rev.get ctxt delegate
                >>=? fun baker ->
                return
                  (Storage.Cycle.Unrevealed {nonce_hash; baker; rewards; fees})
            | Revealed s ->
                return (Storage.Cycle.Revealed s) )
            >>=? fun nonce ->
            (* because the path has not changed, this will override 008 value *)
            Storage.Cycle.Nonce.update (ctxt, cycle) level nonce
            >>=? fun ctxt -> return ctxt)
        >>=? fun ctxt ->
        match Cycle_repr.pred cycle with
        | None ->
            return ctxt
        | Some prev_cycle ->
            migrate_cycle_nonce ctxt prev_cycle
      in
      migrate_cycle_nonce ctxt current_cycle
      >>=? fun ctxt ->
      (* 2. balance update receipts for baker migration *)
      Storage.Baker.Consensus_key_rev.fold
        ctxt
        ~init:(ok [])
        ~f:(fun pkh baker_hash acc ->
          Lwt.return acc
          >>=? fun updates ->
          let original_contract = Contract_repr.implicit_contract pkh in
          let new_contract = Contract_repr.baker_contract baker_hash in
          Storage.Contract.Balance.get ctxt new_contract
          >>=? fun moved_balance ->
          return
          @@ Receipt_repr.
               ( Contract original_contract,
                 Debited moved_balance,
                 Protocol_migration )
             :: Receipt_repr.
                  ( Contract new_contract,
                    Credited moved_balance,
                    Protocol_migration )
             :: updates)
      >>=? fun baker_balance_updates ->
      (* Add balance updates receipts to be attached on the first block of this
         protocol - see [prepare] function below. Any balance updates attached
         in the migration should use the [Receipt_repr.Protocol_migration]
         as their [update_origin].
      *)
      let balance_updates = [] in
      Storage.Pending_migration_balance_updates.init
        ctxt
        (balance_updates @ baker_balance_updates)

let prepare ctxt ~level ~predecessor_timestamp ~timestamp ~fitness =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun ctxt ->
  Storage.Pending_migration_balance_updates.find ctxt
  >>=? function
  | Some balance_updates ->
      Storage.Pending_migration_balance_updates.remove ctxt
      >>= fun ctxt ->
      (* When applying balance updates in a migration, we must attach receipts.
         The balance updates returned from here will be applied in the first
         block of the new protocol. *)
      return (ctxt, balance_updates)
  | None ->
      return (ctxt, [])
