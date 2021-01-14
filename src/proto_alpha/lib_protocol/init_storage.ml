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

let migrate_baker ctxt baker_pkh =
  Contract_storage.get_public_key
    ctxt
    (Contract_repr.implicit_contract baker_pkh)
  >>=? fun pk ->
  let storage = Baker_script_repr.storage ~threshold:1 ~owner_keys:[pk] in
  Baker_storage.fresh_baker_from_current_nonce ctxt
  >>=? fun (ctxt, baker_hash) ->
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
  Storage.Contract.Balance.init ctxt to_contract balance
  >>=? fun ctxt ->
  Storage.Contract.Balance.remove ctxt from_contract
  >>= fun ctxt ->
  (* Roll.Delegate_roll_list -> Roll.Baker_roll_list *)
  Storage.Roll.Delegate_roll_list_008.find ctxt from
  >>=? (function
         | None -> return ctxt
         | Some roll ->
             Storage.Roll.Delegate_roll_list_008.remove ctxt from
             >>= fun ctxt -> Storage.Roll.Baker_roll_list.init ctxt to_ roll)
  >>=? fun ctxt ->
  (* Roll.Delegate_change -> Roll.Baker_change *)
  Storage.Roll.Delegate_change_008.find ctxt from
  >>=? (function
         | None -> return ctxt
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
      Storage.Baker.Frozen_fees.init (ctxt, to_) cycle fee)
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
      Storage.Baker.Frozen_rewards.init (ctxt, to_) cycle rewards)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_rewards_008.clear (ctxt, from_contract)
  >>= fun ctxt -> return (ctxt, (pk, pkh, baker_hash))

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
      let clear_tree ctxt key =
        Raw_context.get_tree ctxt key
        >>=? fun tree ->
        Raw_context.Tree.clear tree;
        return ctxt
      in
      (* 1. Baker accounts migration *)
      lwt_log_notice "1. Baker accounts migration" >>= fun () ->
      let nonce =
        Operation_hash.hash_bytes
          [ Bytes.of_string
              "Things will not calm down, Daniel Jackson. They will, in fact, \
               calm up." ]
      in
      let ctxt = Raw_context.init_origination_nonce ctxt nonce in
      Storage.Delegates_008.fold
        ctxt
        ~init:(ok (ctxt, []))
        ~f:(fun baker_pkh acc ->
          Lwt.return acc
          >>=? fun (ctxt, bakers) ->
          migrate_baker ctxt baker_pkh
          >|=? fun (ctxt, baker) -> (ctxt, baker :: bakers))
      >>=? fun (ctxt, bakers) ->
      Storage.Delegates_008.clear ctxt
      >>= fun ctxt ->
      (* The number of the bakers are small (< 3000).  We keep them
         in memory to speed up the migration *)
      (* prepare leaves of baker hashes *)
      let build_find_baker_hash_tree =
        let module Map = Map.Make(Baker_hash) in
        Lwt_list.fold_left_s (fun map (_,_,bh) ->
            Raw_context.Tree.add
              (Raw_context.Tree.empty ctxt)
              []
              (Data_encoding.Binary.to_bytes_exn Baker_hash.encoding bh)
            >|= fun tree ->
            Map.add bh tree map)
          Map.empty bakers
        >|= fun map ->
        fun bh -> Map.find_opt bh map
      in
      build_find_baker_hash_tree >>= fun find_baker_hash_tree ->
      let find_baker_hash =
        let module Map = Map.Make (struct
          type t = Signature.Public_key_hash.t

          let compare = Signature.Public_key_hash.compare
        end) in
        let map =
          List.fold_left
            (fun m (_pk, pkh, baker_hash) -> Map.add pkh baker_hash m)
            Map.empty
            bakers
        in
        fun pkh -> Map.find_opt pkh map
      in
      let find_baker_hash_result pkh =
        match find_baker_hash pkh with
        | Some x ->
            ok x
        | None ->
            let open Raw_context in
            storage_error
              (Missing_key
                 ( ["baker"; "consensus_key_rev_index"]
                   @ Signature.Public_key_hash.to_path pkh ["consensus_key_rev"],
                   Get ))
      in
      (* public_key binary -> baker_hash *)
      let find_baker_hash_by_pkhbytes =
        let module Map = Map.Make (struct
          type t = bytes
          let compare = Compare.Bytes.compare
        end) in
        let map =
          List.fold_left
            (fun m (_, pkh, baker_hash) ->
              let pkhbytes =
                Data_encoding.Binary.to_bytes_exn
                  Signature.Public_key_hash.encoding
                  pkh
              in
              Map.add pkhbytes baker_hash m)
            Map.empty
            bakers
        in
        fun pkhbytes -> Map.find_opt pkhbytes map
      in
      let current_cycle = Raw_context.(current_level ctxt).cycle in
      (* Take a snapshot of the consensus keys *)
      lwt_log_notice "Take a snapshot of the consensus keys" >>= fun () ->
      Storage.Baker.Consensus_key.snapshot ctxt current_cycle
      >>=? fun ctxt ->
      lwt_log_notice "Take another snapshot of the consensus keys" >>= fun () ->
      (* Because the migration runs on the last block of a predecessor protocol,
         we also need to take the snapshot for the following cycle, otherwise
         the baker process which looks-up consensus for the future block would
         fail. *)
      Storage.Baker.Consensus_key.snapshot ctxt (Cycle_repr.succ current_cycle)
      >>=? fun ctxt ->
      (* Migrate storages with references to original baker pkhs *)
      (* Contract.Delegate *)
      lwt_log_notice
        "Contract.Delegate + Contract.Inactive_delegate + \
         Contract.Delegate_desactivation"
      >>= fun () ->
      let map_contracts_index ctxt ~init f =
        let index_key = ["contracts"; "index"] in
        Raw_context.get_tree ctxt index_key
        >>=? fun index_tree ->
        Raw_context.Tree.fold
          index_tree
          []
          ~depth:(`Eq 7)
          ~init:(0, index_tree, init)
          ~f:(fun key tree (nmod, index_tree, acc) ->
              f key tree acc
              >>= fun (modified, tree, acc) ->
              ( if modified then Raw_context.Tree.add_tree index_tree key tree
              else Lwt.return index_tree )
              >|= fun index_tree ->
              ((if modified then nmod + 1 else nmod), index_tree, acc))
        >>= fun (nmod, index_tree, acc) ->
        Raw_context.update_tree ctxt index_key index_tree
        >>=? fun ctxt ->
        clear_tree ctxt index_key
        >|=? fun ctxt ->
        (ctxt, nmod, acc)
      in
      map_contracts_index
        ctxt
        ~init:(0, [], [])
        (fun key tree (delegates, inactive_bakers, desactivations) ->
          let get_baker_res () =
            let contract =
              match Contract_repr.Index.of_path key with
              | None ->
                  assert false
              | Some contract ->
                  contract
            in
            match Contract_repr.is_implicit contract with
            | None ->
                Error contract
            | Some baker ->
                Ok baker
          in
          let get_baker () =
            match get_baker_res () with
            | Error contract ->
                let message =
                  Format.asprintf
                    "baker \"%a\" is not implicit account"
                    Contract_repr.pp
                    contract
                in
                failwith message
            | Ok baker ->
                baker
          in
          (* Contract.Delegate *)
          let delegate_key = ["delegate"] in
          Raw_context.Tree.find tree delegate_key
          >>= (function
                | None ->
                    Lwt.return (false, tree, delegates)
                | Some pkh_bytes ->
                    let delegate =
                      match find_baker_hash_by_pkhbytes pkh_bytes with
                      | None ->
                          (* NOTE There's one contract
                             "tz1gNQpjio7F5HsR9bs37vBYZXfbWpZ34tpL" that's not registered
                             as baker, but because of a bug in protocol 001 it has two
                             originated contracts delegated to it:
                              - KT1LLKuQLmxciFUif4yt6UyXFqEE3235emHE
                              - KT19MYkajshb4DnpDyNC37Qm7DCbPh5GP5Sg
                             We drop the delegations here.
                          *)
                          None
                      | Some baker_hash -> (
                        match get_baker_res () with
                        | Error _ ->
                            Some baker_hash
                        | Ok contract_pkh -> (
                          (* Check this the contract is a baker *)
                          match find_baker_hash contract_pkh with
                          | None ->
                              Some baker_hash
                          | Some _ ->
                              (* Baker contracts are no longer self-delegated, so we
                               don't migrate this relation, instead we delete it *)
                              None ) )
                    in
                    let delegate_tree =
                      Option.map (fun bh ->
                          match find_baker_hash_tree bh with
                          | None -> assert false (* never fails *)
                          | Some bht -> bht) delegate
                    in
                    Raw_context.Tree.add_or_remove_tree
                      tree delegate_key delegate_tree
                    >|= fun tree -> (true, tree, delegates + 1))
          >>= fun (modded, tree, delegates) ->
          (* Contract.Inactive_delegate -> Baker.Inactive *)
          let inactive_delegate_key = ["inactive_delegate"] in
          Raw_context.Tree.find tree inactive_delegate_key
          >>= (function
                | None ->
                    Lwt.return (modded, tree, inactive_bakers) (* no update *)
                | Some _ (* "inited" *) -> (
                    let baker = get_baker () in
                    match find_baker_hash baker with
                    | None ->
                        assert false
                    | Some migrated_baker -> (
                        let inactive_bakers =
                          migrated_baker :: inactive_bakers
                        in
                        Raw_context.Tree.remove_existing
                          tree
                          inactive_delegate_key
                        >>= function
                        | Error _ ->
                            assert false
                        | Ok tree ->
                            Lwt.return (true, tree, inactive_bakers) ) ))
          >>= fun (moded, tree, inactive_bakers) ->
          (* Contract.Delegate_desactivation -> Baker.Inactive *)
          let delegate_desactivation_key = ["delegate_desactivation"] in
          Raw_context.Tree.find tree delegate_desactivation_key
          >>= (function
                | None ->
                    Lwt.return (moded, tree, desactivations) (* no update *)
                | Some cycle_bytes -> (
                    let cycle =
                      match
                        Data_encoding.Binary.of_bytes
                          Cycle_repr.encoding
                          cycle_bytes
                      with
                      | None ->
                          assert false
                      | Some cycle ->
                          cycle
                    in
                    let baker = get_baker () in
                    match find_baker_hash baker with
                    | None ->
                        assert false
                    | Some migrated_baker -> (
                        let desactivations =
                          (migrated_baker, cycle) :: desactivations
                        in
                        Raw_context.Tree.remove_existing
                          tree
                          delegate_desactivation_key
                        >>= function
                        | Error _ ->
                            assert false
                        | Ok tree ->
                            Lwt.return (true, tree, desactivations) ) ))
          >|= fun (moded, tree, desactivations) ->
          (moded, tree, (delegates, inactive_bakers, desactivations)))
      >>=? fun (ctxt, nmod, (delegates, inactive_bakers, desactivations)) ->
      lwt_log_notice "%d contracts updated" nmod
      >>= fun () ->
      lwt_log_notice "%d Cotract.Delegate updated" delegates
      >>= fun () ->
      Lwt_list.fold_left_s
        (fun ctxt baker_hash -> Storage.Baker.Inactive.add ctxt baker_hash)
        ctxt
        inactive_bakers
      >>= fun ctxt ->
      lwt_log_notice
        "%d Contract.Inactive_delegate updated"
        (List.length inactive_bakers)
      >>= fun () ->
      Lwt_list.fold_left_s
        (fun ctxt (migrated_baker, cycle) ->
          Storage.Baker.Deactivation.add ctxt migrated_baker cycle)
        ctxt
        desactivations
      >>= fun ctxt ->
      lwt_log_notice
        "%d Contract.Delegate_desactivations updated"
        (List.length desactivations)
      >>= fun () ->
      (* Active_delegates_with_rolls -> Baker.Active_with_rolls *)
      lwt_log_notice "Active_delegates_with_rolls -> Baker.Active_with_rolls" >>= fun () ->
      Storage.Active_delegates_with_rolls_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Baker.Active_with_rolls.add ctxt baker >|= ok)
      >>=? fun ctxt ->
      Storage.Active_delegates_with_rolls_008.clear ctxt
      >>= fun ctxt ->
      let preserved = Constants_storage.preserved_cycles ctxt in
      (* Delegates_with_frozen_balance -> Baker.With_frozen_balance *)
      lwt_log_notice "Delegates_with_frozen_balance -> Baker.With_frozen_balance" >>= fun () ->
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
            Lwt.return (find_baker_hash_result pkh)
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
      (* public_key binary -> baker_hash *)
      let find_baker_hash_by_pkbytes =
        let module Map = Map.Make (struct
          type t = bytes

          let compare = Compare.Bytes.compare
        end) in
        let map =
          List.fold_left
            (fun m (pk, _, baker_hash) ->
              let pkbytes =
                Data_encoding.Binary.to_bytes_exn
                  Signature.Public_key.encoding
                  pk
              in
              Map.add pkbytes baker_hash m)
            Map.empty
            bakers
        in
        fun pkbytes -> Map.find_opt pkbytes map
      in
      let rewrite_rolls ctxt key depth =
        Raw_context.get_tree ctxt []
        >>=? fun root_tree ->
        Raw_context.Tree.fold
          root_tree
          key
          ~depth:(`Eq depth)
          ~init:(0,Raw_context.Tree.empty ctxt)
          ~f:(fun key pkbytes_tree (i,acc) ->
              Raw_context.Tree.to_value pkbytes_tree
              >>= function
              | None -> assert false
              | Some pkbytes ->
                  match find_baker_hash_by_pkbytes pkbytes with
                  | None ->
                      assert false
                  | Some baker_hash ->
                      Raw_context.Tree.add_tree
                        acc
                        key
                        (match find_baker_hash_tree baker_hash with
                         | None -> assert false (* never fails *)
                         | Some bht -> bht)
                      >|= fun acc ->
                      (i+1,acc))
        >>= fun (i,tree_snapshot) ->
        Raw_context.update_tree ctxt key tree_snapshot
        >|=? fun ctxt ->
        Raw_context.Tree.clear root_tree;
        (i,ctxt)
      in
      (* Roll.Owner *)
      lwt_log_notice "Roll.Owner"
      >>= fun () ->
      rewrite_rolls ctxt ["rolls"; "owner"; "current"] 3
      >>=? fun (n,ctxt) ->
      lwt_log_notice "Roll.Owner: %d" n
      >>= fun () ->
      lwt_log_notice "Roll.Owner.Snapshot"
      >>= fun () ->
      Raw_context.fold
        ~depth:(`Eq 2)
        ctxt
        ["rolls"; "owner"; "snapshot"]
        ~init:[]
        ~f:(fun k _ ks -> Lwt.return (k :: ks))
      >>= fun ks ->
      Lwt_list.fold_left_s
        (fun acc k ->
          Lwt.return acc
          >>=? fun ctxt ->
          lwt_log_notice "Roll.Owner.Snapshot %s" (String.concat "/" k)
          >>= fun () ->
          rewrite_rolls ctxt (["rolls"; "owner"; "snapshot"] @ k) 3
          >>=? fun (n,ctxt) ->
          lwt_log_notice "Roll.Owner.Snapshot %s: %d" (String.concat "/" k) n
          >|= fun () ->
          ok ctxt)
        (ok ctxt)
        ks
      >>=? fun ctxt ->
      (* Vote.Listings *)
      lwt_log_notice "Vote.Listings" >>= fun () ->
      Storage.Vote.Listings_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh listings acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Listings_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Listings.init ctxt baker listings)
      >>=? fun ctxt ->
      (* Vote.Proposals *)
      lwt_log_notice "Vote.Proposals" >>= fun () ->
      Storage.Vote.Proposals_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun (proposal, pkh) acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Proposals_008.remove ctxt (proposal, pkh)
          >>= fun ctxt ->
          Storage.Vote.Proposals.add ctxt (proposal, baker)
          >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      (* Vote.Proposals_count *)
      lwt_log_notice "Vote.Proposals_count" >>= fun () ->
      Storage.Vote.Proposals_count_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh count acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Proposals_count_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Proposals_count.init ctxt baker count)
      >>=? fun ctxt ->
      (* Vote.Ballots *)
      lwt_log_notice "Vote.Ballots" >>= fun () ->
      Storage.Vote.Ballots_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh ballot acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Ballots_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Ballots.init ctxt baker ballot)
      >>=? fun ctxt ->
      (* Storage.Seed.Nonce *)
      lwt_log_notice "Storage.Seed.Nonce" >>= fun () ->
      let rec migrate_cycle_nonce ctxt cycle =
        Storage.Cycle_008.Nonce.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun level nonce acc ->
            Lwt.return acc
            >>=? fun ctxt ->
            ( match nonce with
            | Unrevealed {nonce_hash; delegate; rewards; fees} ->
                Lwt.return (find_baker_hash_result delegate)
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
      lwt_log_notice "2. balance update receipts for baker migration" >>= fun () ->
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
      lwt_log_notice "Add balance updates receipts" >>= fun () ->
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
