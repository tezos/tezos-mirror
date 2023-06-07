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

(*
  To patch code of legacy contracts you can add a helper function here and call
  it at the end of prepare_first_block.

  See !3730 for an example.
*)

let patch_script ctxt (address, hash, patched_code) =
  Contract_repr.of_b58check address >>?= fun contract ->
  Storage.Contract.Code.find ctxt contract >>=? fun (ctxt, code_opt) ->
  Logging.log Notice "Patching %s... " address ;
  match code_opt with
  | Some old_code ->
      let old_bin = Data_encoding.force_bytes old_code in
      let old_hash = Script_expr_hash.hash_bytes [old_bin] in
      if Script_expr_hash.equal old_hash hash then (
        let new_code = Script_repr.lazy_expr patched_code in
        Storage.Contract.Code.update ctxt contract new_code
        >>=? fun (ctxt, size_diff) ->
        Logging.log Notice "Contract %s successfully patched" address ;
        let size_diff = Z.of_int size_diff in
        Storage.Contract.Used_storage_space.get ctxt contract
        >>=? fun prev_size ->
        let new_size = Z.add prev_size size_diff in
        Storage.Contract.Used_storage_space.update ctxt contract new_size
        >>=? fun ctxt ->
        if Z.(gt size_diff zero) then
          Storage.Contract.Paid_storage_space.get ctxt contract
          >>=? fun prev_paid_size ->
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

(** Converts {Storage.Stake.Total_active_stake} and
    {Storage.Stake.Selected_distribution_for_cycle} from {Tez_repr} to
    {Stake_repr}.
    Remove me in P. *)
let migrate_stake_distribution_for_o ctxt =
  let open Lwt_result_syntax in
  let convert =
    let delegation_over_baking_limit =
      Constants_storage.delegation_over_baking_limit ctxt
    in
    fun old_stake ->
      let frozen =
        Tez_repr.div_exn old_stake (delegation_over_baking_limit + 1)
      in
      match Tez_repr.sub_opt old_stake frozen with
      | Some delegated -> Stake_repr.make ~frozen ~delegated
      | None -> Stake_repr.make ~frozen:old_stake ~delegated:Tez_repr.zero
  in
  let* ctxt =
    Storage.Stake.Total_active_stake_up_to_Nairobi.fold
      ctxt
      ~order:`Undefined
      ~init:(ok ctxt)
      ~f:(fun cycle stake ctxt ->
        let*? ctxt in
        let stake = convert stake in
        Storage.Stake.Total_active_stake.update ctxt cycle stake)
  in
  Storage.Stake.Selected_distribution_for_cycle_up_to_Nairobi.fold
    ctxt
    ~order:`Undefined
    ~init:(ok ctxt)
    ~f:(fun cycle distr ctxt ->
      let*? ctxt in
      let distr = List.map (fun (pkh, stake) -> (pkh, convert stake)) distr in
      Storage.Stake.Selected_distribution_for_cycle.update ctxt cycle distr)

(** Initializes the total supply at the beginning of O
    Uses an estimation of the total supply at the activation of O.
    This value can be refined at the beginning of P to have a
    perfectly accurate ammount.

    Remove me in P. *)
let initialize_total_supply_for_o ctxt =
  Storage.Contract.Total_supply.add
    ctxt
    (Tez_repr.of_mutez_exn 940_000_000_000_000L)

(** Migration of the context field storing the Liquidity Baking
    EMA. The key of the field is renamed from
    "liquidity_baking_escape_ema" to "liquidity_baking_toggle_ema" and
    it is now stored on an int64 instead of an int32. *)
let migrate_liquidity_baking_ema ctxt =
  let legacy_key = ["liquidity_baking_escape_ema"] in
  let get_and_remove_legacy_field ctxt =
    Raw_context.get ctxt legacy_key >>=? fun ema_bytes ->
    Raw_context.remove_existing ctxt legacy_key >>=? fun ctxt ->
    return (ctxt, ema_bytes)
  in
  (get_and_remove_legacy_field ctxt >|= function
   | Ok (ctxt, ema_bytes) -> (
       match
         Data_encoding.Binary.of_bytes_opt Data_encoding.int32 ema_bytes
       with
       | Some ema_i32 -> (ctxt, ema_i32)
       | None ->
           Logging.log
             Logging.Error
             "Migration of Liquidity Baking EMA: decoding error, the field %S \
              was expected to be an int32 but it is %a. Defaulting to 0."
             (String.concat "/" legacy_key)
             Hex.pp
             (Hex.of_bytes ema_bytes) ;
           (ctxt, 0l))
   | Error trace ->
       Logging.log
         Logging.Error
         "%a@.Migration of Liquidity Baking EMA: the key %S was not found in \
          context so defaulting to 0."
         pp_trace
         trace
         (String.concat "/" legacy_key) ;
       (ctxt, 0l))
  >>= fun (ctxt, ema_i32) ->
  Storage.Liquidity_baking.Toggle_ema.init ctxt (Int64.of_int32 ema_i32)

let prepare_first_block _chain_id ctxt ~typecheck_smart_contract
    ~typecheck_smart_rollup ~level ~timestamp ~predecessor =
  Raw_context.prepare_first_block ~level ~timestamp ctxt
  >>=? fun (previous_protocol, ctxt) ->
  let parametric = Raw_context.constants ctxt in
  ( Raw_context.Cache.set_cache_layout
      ctxt
      (Constants_repr.cache_layout parametric)
  >|= fun ctxt -> Raw_context.Cache.clear ctxt )
  >>= fun ctxt ->
  (match previous_protocol with
  | Genesis param ->
      (* This is the genesis protocol: initialise the state *)
      Raw_level_repr.of_int32 level >>?= fun level ->
      Storage.Tenderbake.First_level_of_protocol.init ctxt level
      >>=? fun ctxt ->
      Storage.Tenderbake.Forbidden_delegates.init
        ctxt
        Signature.Public_key_hash.Set.empty
      >>=? fun ctxt ->
      Storage.Contract.Total_supply.add ctxt Tez_repr.zero >>= fun ctxt ->
      Storage.Block_round.init ctxt Round_repr.zero >>=? fun ctxt ->
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
        ~typecheck_smart_contract
        ~typecheck_smart_rollup
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
        param.bootstrap_smart_rollups
      >>=? fun (ctxt, bootstrap_balance_updates) ->
      Delegate_cycles.init_first_cycles ctxt ~origin:Protocol_migration
      >>=? fun (ctxt, deposits_balance_updates) ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Vote_storage.update_listings ctxt >>=? fun ctxt ->
      (* Must be called after other originations since it unsets the origination nonce. *)
      Liquidity_baking_migration.init ctxt ~typecheck:typecheck_smart_contract
      >>=? fun (ctxt, operation_results) ->
      Storage.Pending_migration.Operation_results.init ctxt operation_results
      >>=? fun ctxt ->
      Sc_rollup_inbox_storage.init_inbox ~predecessor ctxt >>=? fun ctxt ->
      Adaptive_inflation_storage.init_ema ctxt >>=? fun ctxt ->
      return
        ( ctxt,
          commitments_balance_updates @ bootstrap_balance_updates
          @ deposits_balance_updates )
  | Nairobi_017
  (* Please update [next_protocol] and [previous_protocol] in
     [tezt/lib_tezos/protocol.ml] when you update this value. *) ->
      (* TODO (#2704): possibly handle endorsements for migration block (in bakers);
         if that is done, do not set Storage.Tenderbake.First_level_of_protocol.
         /!\ this storage is also use to add the smart rollup
             inbox migration message. see `sc_rollup_inbox_storage`. *)
      Raw_level_repr.of_int32 level >>?= fun level ->
      Storage.Tenderbake.First_level_of_protocol.update ctxt level
      >>=? fun ctxt ->
      Storage.Tenderbake.Forbidden_delegates.init
        ctxt
        Signature.Public_key_hash.Set.empty
      >>=? fun ctxt ->
      migrate_stake_distribution_for_o ctxt >>=? fun ctxt ->
      initialize_total_supply_for_o ctxt >>= fun ctxt ->
      Remove_zero_amount_ticket_migration_for_o.remove_zero_ticket_entries ctxt
      >>= fun ctxt ->
      migrate_liquidity_baking_ema ctxt >>=? fun ctxt ->
      Adaptive_inflation_storage.init_ema ctxt >>=? fun ctxt -> return (ctxt, []))
  >>=? fun (ctxt, balance_updates) ->
  List.fold_left_es patch_script ctxt Legacy_script_patches.addresses_to_patch
  >>=? fun ctxt ->
  Receipt_repr.group_balance_updates balance_updates >>?= fun balance_updates ->
  Storage.Pending_migration.Balance_updates.add ctxt balance_updates
  >>= fun ctxt -> return ctxt

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  >>=? fun ctxt -> Storage.Pending_migration.remove ctxt
