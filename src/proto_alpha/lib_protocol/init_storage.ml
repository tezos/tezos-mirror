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
  let open Lwt_result_syntax in
  match Tez_repr.of_mutez amount_mutez with
  | None -> Lwt.return (ctxt, [])
  | Some amount -> (
      let*! result =
        let*? recipient = Contract_repr.of_b58check address in
        Token.transfer
          ~origin:Protocol_migration
          ctxt
          `Invoice
          (`Contract recipient)
          amount
      in
      Lwt.return @@ match result with Ok res -> res | Error _ -> (ctxt, []))
*)

(*
  To patch code of legacy contracts you can add a helper function here and call
  it at the end of prepare_first_block.

  See !3730 for an example.
*)

let patch_script ctxt (address, hash, patched_code) =
  let open Lwt_result_syntax in
  let*? contract = Contract_repr.of_b58check address in
  let* ctxt, code_opt = Storage.Contract.Code.find ctxt contract in
  Logging.log Notice "Patching %s... " address ;
  match code_opt with
  | Some old_code ->
      let old_bin = Data_encoding.force_bytes old_code in
      let old_hash = Script_expr_hash.hash_bytes [old_bin] in
      if Script_expr_hash.equal old_hash hash then (
        let new_code = Script_repr.lazy_expr patched_code in
        let* ctxt, size_diff =
          Storage.Contract.Code.update ctxt contract new_code
        in
        Logging.log Notice "Contract %s successfully patched" address ;
        let size_diff = Z.of_int size_diff in
        let* prev_size =
          Storage.Contract.Used_storage_space.get ctxt contract
        in
        let new_size = Z.add prev_size size_diff in
        let* ctxt =
          Storage.Contract.Used_storage_space.update ctxt contract new_size
        in
        if Z.(gt size_diff zero) then
          let* prev_paid_size =
            Storage.Contract.Paid_storage_space.get ctxt contract
          in
          let paid_size = Z.add prev_paid_size size_diff in
          Storage.Contract.Paid_storage_space.update ctxt contract paid_size
        else return ctxt)
      else (
        Logging.log
          Error
          "Patching %s was skipped because its script does not have the \
           expected hash (expected: %a, found: %a)"
          address
          Script_expr_hash.pp
          hash
          Script_expr_hash.pp
          old_hash ;
        return ctxt)
  | None ->
      Logging.log
        Error
        "Patching %s was skipped because no script was found for it in the \
         context."
        address ;
      return ctxt

let migrate_already_denounced_from_Oxford ctxt =
  let open Lwt_syntax in
  let migrate_cycle ctxt cycle =
    let* ctxt =
      Storage.Already_denounced__Oxford.fold
        (ctxt, cycle)
        ~order:`Undefined
        ~init:ctxt
        ~f:(fun (level, delegate) {for_double_attesting; for_double_baking} ctxt
           ->
          Storage.Already_denounced.add
            (ctxt, cycle)
            ((level, Round_repr.zero), delegate)
            {
              for_double_preattesting = for_double_attesting;
              for_double_attesting;
              for_double_baking;
            })
    in
    Storage.Already_denounced__Oxford.clear (ctxt, cycle)
  in
  (* Since the max_slashing_period is 2, denunciations are only
     relevant if the misbehaviour happened in either the current cycle
     or the previous cycle. *)
  let current_cycle = (Level_storage.current ctxt).cycle in
  let* ctxt = migrate_cycle ctxt current_cycle in
  match Cycle_repr.pred current_cycle with
  | None -> return ctxt
  | Some previous_cycle -> migrate_cycle ctxt previous_cycle

(* This removes snapshots and moves the current [staking_balance] one level
   up. Same thing for active delegates with minimal stake but renames the key
   at the same time. *)
let migrate_staking_balance_and_active_delegates_for_p ctxt =
  let open Lwt_result_syntax in
  let* staking_balance_tree =
    Raw_context.get_tree ctxt ["staking_balance"; "current"]
  in
  let*! ctxt =
    Raw_context.add_tree ctxt ["staking_balance"] staking_balance_tree
  in
  let* active_delegates_tree =
    Raw_context.get_tree ctxt ["active_delegate_with_one_roll"; "current"]
  in
  let*! ctxt = Raw_context.remove ctxt ["active_delegate_with_one_roll"] in
  let*! ctxt =
    Raw_context.add_tree
      ctxt
      ["active_delegates_with_minimal_stake"]
      active_delegates_tree
  in
  return ctxt

(* This should clean most of the remaining fields [frozen_deposits].
   The field was removed in Oxford but cleaning it using [remove] was too
   costly as it iterated over all contracts.
   Instead we iterate over activate delegates.
   If there are remaining [frozen_deposits] once P activates, they can still be
   removed one by one in Q. *)
let clean_frozen_deposits_for_p ctxt =
  let open Lwt_result_syntax in
  let contracts_index = ["contracts"; "index"] in
  let* contracts_tree = Raw_context.get_tree ctxt contracts_index in
  let field = ["frozen_deposits"] in
  let*! contracts_tree =
    Storage.Stake.Active_delegates_with_minimal_stake.fold
      ctxt
      ~order:`Undefined
      ~init:contracts_tree
      ~f:(fun pkh contracts_tree ->
        let path = Contract_repr.Index.to_path (Implicit pkh) field in
        Raw_context.Tree.remove contracts_tree path)
  in
  Raw_context.update_tree ctxt contracts_index contracts_tree

let cleanup_values_for_protocol_p ctxt
    (previous_proto_constants : Constants_parametric_previous_repr.t option)
    level =
  let open Lwt_result_syntax in
  let preserved_cycles =
    let previous_proto_constants =
      match previous_proto_constants with
      | None ->
          (* Shouldn't happen *)
          failwith
            "Internal error: cannot read previous protocol constants in \
             context."
      | Some c -> c
    in
    previous_proto_constants.preserved_cycles
  in
  let consensus_rights_delay = Constants_storage.consensus_rights_delay ctxt in
  let new_cycle =
    let next_level = Raw_level_repr.succ level in
    let cycle_eras = Raw_context.cycle_eras ctxt in
    (Level_repr.level_from_raw ~cycle_eras next_level).cycle
  in
  let* ctxt =
    Stake_storage.cleanup_values_for_protocol_p
      ctxt
      ~preserved_cycles
      ~consensus_rights_delay
      ~new_cycle
  in
  let* ctxt =
    Delegate_sampler.cleanup_values_for_protocol_p
      ctxt
      ~preserved_cycles
      ~consensus_rights_delay
      ~new_cycle
  in
  return ctxt

let prepare_first_block chain_id ctxt ~typecheck_smart_contract
    ~typecheck_smart_rollup ~level ~timestamp ~predecessor =
  let open Lwt_result_syntax in
  let* previous_protocol, previous_proto_constants, ctxt =
    Raw_context.prepare_first_block ~level ~timestamp chain_id ctxt
  in
  let parametric = Raw_context.constants ctxt in
  let*! ctxt =
    let*! ctxt =
      Raw_context.Cache.set_cache_layout
        ctxt
        (Constants_repr.cache_layout parametric)
    in
    Lwt.return (Raw_context.Cache.clear ctxt)
  in
  let* ctxt, balance_updates =
    match previous_protocol with
    | Genesis param ->
        (* This is the genesis protocol: initialise the state *)
        let*? level = Raw_level_repr.of_int32 level in
        let* ctxt =
          Storage.Tenderbake.First_level_of_protocol.init ctxt level
        in
        let* ctxt = Forbidden_delegates_storage.init_for_genesis ctxt in
        let*! ctxt = Storage.Contract.Total_supply.add ctxt Tez_repr.zero in
        let* ctxt = Storage.Block_round.init ctxt Round_repr.zero in
        let init_commitment (ctxt, balance_updates)
            Commitment_repr.{blinded_public_key_hash; amount} =
          let* ctxt, new_balance_updates =
            Token.transfer
              ctxt
              `Initial_commitments
              (`Collected_commitments blinded_public_key_hash)
              amount
          in
          return (ctxt, new_balance_updates @ balance_updates)
        in
        let* ctxt, commitments_balance_updates =
          List.fold_left_es init_commitment (ctxt, []) param.commitments
        in
        let* ctxt =
          Seed_storage.init ?initial_seed:param.constants.initial_seed ctxt
        in
        let* ctxt = Contract_storage.init ctxt in
        let* ctxt, bootstrap_balance_updates =
          Bootstrap_storage.init
            ctxt
            ~typecheck_smart_contract
            ~typecheck_smart_rollup
            ?no_reward_cycles:param.no_reward_cycles
            param.bootstrap_accounts
            param.bootstrap_contracts
            param.bootstrap_smart_rollups
        in
        let* ctxt = Delegate_cycles.init_first_cycles ctxt in
        let* ctxt =
          Vote_storage.init
            ctxt
            ~start_position:(Level_storage.current ctxt).level_position
        in
        let* ctxt = Vote_storage.update_listings ctxt in
        (* Must be called after other originations since it unsets the origination nonce. *)
        let* ctxt, operation_results =
          Liquidity_baking_migration.init
            ctxt
            ~typecheck:typecheck_smart_contract
        in
        let* ctxt =
          Storage.Pending_migration.Operation_results.init
            ctxt
            operation_results
        in
        let* ctxt = Sc_rollup_inbox_storage.init_inbox ~predecessor ctxt in
        let* ctxt = Adaptive_issuance_storage.init ctxt in
        return (ctxt, commitments_balance_updates @ bootstrap_balance_updates)
    | Oxford_018
    (* Please update [next_protocol] and [previous_protocol] in
       [tezt/lib_tezos/protocol.ml] when you update this value. *) ->
        (* TODO (#2704): possibly handle attestations for migration block (in bakers);
           if that is done, do not set Storage.Tenderbake.First_level_of_protocol.
           /!\ this storage is also use to add the smart rollup
               inbox migration message. see `sc_rollup_inbox_storage`. *)
        let*? level = Raw_level_repr.of_int32 level in
        let* ctxt =
          Storage.Tenderbake.First_level_of_protocol.update ctxt level
        in
        (* Migration of refutation games needs to be kept for each protocol. *)
        let* ctxt =
          Sc_rollup_refutation_storage.migrate_clean_refutation_games ctxt
        in
        (* Adaptive Issuance-related migrations from Oxford to P. *)
        (* We usually clear the table at the end of the cycle but the migration
           can happen in the middle of the cycle, so we clear it here.
           Possible consequence: the slashing history could be inconsistent with
           the pending denunciations, i.e., there could be unstaked_frozen_deposits
           that are not slashed whereas unstake_requests are slashed. *)
        let*! ctxt = Pending_denunciations_storage.clear ctxt in
        let*! ctxt = migrate_already_denounced_from_Oxford ctxt in
        let* ctxt = migrate_staking_balance_and_active_delegates_for_p ctxt in
        let* ctxt = clean_frozen_deposits_for_p ctxt in
        let*! ctxt = Raw_context.remove ctxt ["last_snapshot"] in
        let* ctxt =
          cleanup_values_for_protocol_p ctxt previous_proto_constants level
        in
        (* Update the percentage representation of the stored slashes *)
        let*! ctxt =
          Delegate_slashed_deposits_storage.update_slashing_storage_for_p ctxt
        in
        return (ctxt, [])
  in
  let* ctxt =
    List.fold_left_es patch_script ctxt Legacy_script_patches.addresses_to_patch
  in
  let*? balance_updates = Receipt_repr.group_balance_updates balance_updates in
  let*! ctxt =
    Storage.Pending_migration.Balance_updates.add ctxt balance_updates
  in
  if Constants_storage.adaptive_issuance_force_activation ctxt then
    let ctxt = Raw_context.set_adaptive_issuance_enable ctxt in
    let* ctxt =
      let current_cycle = (Level_storage.current ctxt).cycle in
      Storage.Adaptive_issuance.Activation.update ctxt (Some current_cycle)
    in
    return ctxt
  else return ctxt

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  let open Lwt_result_syntax in
  let* ctxt =
    Raw_context.prepare
      ~level
      ~predecessor_timestamp
      ~timestamp
      ~adaptive_issuance_enable:false
      ctxt
  in
  let* ctxt = Adaptive_issuance_storage.set_adaptive_issuance_enable ctxt in
  Storage.Pending_migration.remove ctxt
