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

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  let open Lwt_result_syntax in
  let* ctxt =
    Raw_context.prepare
      ~level
      ~predecessor_timestamp
      ~timestamp
      ~all_bakers_attest_first_level:None
      ctxt
  in
  let* ctxt =
    All_bakers_attest_activation_storage.set_all_bakers_attest_first_level ctxt
  in
  Storage.Pending_migration.remove ctxt

(* Start of code to remove at next automatic protocol snapshot *)

(* Please add here any code that should be removed at the next automatic protocol snapshot *)

(* End of code to remove at next automatic protocol snapshot *)

let initialize_accumulator_native_contract ctxt =
  let open Lwt_result_syntax in
  (* Please note that this is the same mecanism for originating the Liquidity
     Baking contract. *)
  let operation_hash =
    Operation_hash.hash_string
      (* A contract hash is generated from an origination nonce, which is itself
         generated from the operation hash that contains its origination. As we
         cannot originate it from an operation, we then must use a random seed.
         This text is part of `Celestial Dew` description from Elden Ring: this
         is a placeholder for now, let's maybe find something better (or that
         produces a better KT1 hash). *)
      [
        "Once upon a time, the stars of the night sky guided fate, and this is \
         a recollection of those times. ";
      ]
  in
  let ctxt = Raw_context.init_origination_nonce ctxt operation_hash in
  let*? ctxt, contract_hash =
    Contract_storage.fresh_contract_from_current_nonce ctxt
  in
  let* ctxt =
    Contract_storage.native_originate
      ctxt
      contract_hash
      ~script:(Script_native_repr.CLST_contract.with_initial_storage, None)
  in
  let* ctxt = Storage.Contract.Native_contracts.CLST.init ctxt contract_hash in
  return (Raw_context.unset_origination_nonce ctxt)

let prepare_first_block chain_id ctxt ~typecheck_smart_contract
    ~typecheck_smart_rollup ~level ~timestamp ~predecessor =
  let open Lwt_result_syntax in
  let* previous_protocol, previous_proto_constants, ctxt =
    Raw_context.prepare_first_block ~level ~timestamp chain_id ctxt
  in
  let*? level = Raw_level_repr.of_int32 level in
  (* To gracefully handle time block change, we keep a record of all commitment
     periods used in the past. *)
  let* ctxt =
    match previous_proto_constants with
    | Some previous_proto_constants ->
        let* ctxt =
          Sc_rollup_storage.save_commitment_period
            ctxt
            previous_proto_constants.sc_rollup.commitment_period_in_blocks
            level
        in
        Dal_storage.save_parameters
          ctxt
          previous_proto_constants.dal
          ~next_protocol_activation:level
    | None -> return ctxt
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
        let* ctxt = Storage.Protocol_activation_level.init ctxt level in
        let* ctxt = Forbidden_delegates_storage.init_for_genesis ctxt in
        let*! ctxt = Storage.Contract.Total_supply.add ctxt Tez_repr.zero in
        let* ctxt = Storage.Block_round.init ctxt Round_repr.zero in
        let* ctxt = Storage.Consecutive_round_zero.init ctxt 0l in
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
        let* ctxt = Adaptive_issuance_storage.init_from_genesis ctxt in
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
        let* ctxt = Address_registry_storage.init ctxt in
        let* ctxt =
          if parametric.native_contracts_enable then
            initialize_accumulator_native_contract ctxt
          else return ctxt
        in
        return (ctxt, commitments_balance_updates @ bootstrap_balance_updates)
        (* Start of Alpha stitching. Comment used for automatic snapshot *)
    | Alpha ->
        let* ctxt = Storage.Protocol_activation_level.update ctxt level in
        (* Migration of refutation games needs to be kept for each protocol. *)
        let* ctxt =
          Sc_rollup_refutation_storage.migrate_clean_refutation_games ctxt
        in
        return (ctxt, [])
        (* End of Alpha stitching. Comment used for automatic snapshot *)
        (* Start of alpha predecessor stitching. Comment used for automatic snapshot *)
    | T024 ->
        let* ctxt =
          let*! ctxt = Storage.Tenderbake.First_level_of_protocol.remove ctxt in
          Storage.Protocol_activation_level.init ctxt level
        in
        (* Migration of refutation games needs to be kept for each protocol. *)
        let* ctxt =
          Sc_rollup_refutation_storage.migrate_clean_refutation_games ctxt
        in
        return (ctxt, [])
    (* End of alpha predecessor stitching. Comment used for automatic snapshot *)
  in
  let* ctxt =
    List.fold_left_es patch_script ctxt Legacy_script_patches.addresses_to_patch
  in
  let*? balance_updates = Receipt_repr.group_balance_updates balance_updates in
  let*! ctxt =
    Storage.Pending_migration.Balance_updates.add ctxt balance_updates
  in
  return ctxt
