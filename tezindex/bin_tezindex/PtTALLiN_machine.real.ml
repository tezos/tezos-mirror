(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Block_services =
  Tezos_client_024_PtTALLiN.Protocol_client_context.Alpha_block_services

open Lwt_result_syntax
open Tezos_protocol_024_PtTALLiN
open Tezos_protocol_plugin_024_PtTALLiN

module Services : Protocol_machinery.PROTOCOL_SERVICES = struct
  let hash = Protocol.hash

  type wrap_full = Tezos_client_024_PtTALLiN.Protocol_client_context.wrap_full

  let wrap_full cctxt =
    new Tezos_client_024_PtTALLiN.Protocol_client_context.wrap_full cctxt

  type update =
    | Block_fees
    | Block_rewards
    | Block_bonuses
    | Attestation_rewards
    | Dal_attestation_rewards
    | Contract of Protocol.Alpha_context.Contract.t
    | Baker of Signature.public_key_hash
    | Staker of Protocol.Alpha_context.Contract.t * Signature.public_key_hash
    | Shared of Signature.public_key_hash
    | Edge of Signature.public_key_hash
    | Lost_attestation of Signature.public_key_hash
    | Lost_dal_attestation of Signature.public_key_hash

  let get_balance_update
      (Balance_update_item (balance, balance_update, origin) :
        Protocol.Alpha_context.Receipt.balance_update_item) =
    let get_tz tz = Protocol.Alpha_context.Tez.to_mutez tz in
    match (balance, balance_update, origin) with
    (* Block_application - Tracked debited sources *)
    | Block_fees, Debited tz, Block_application -> Some (Block_fees, get_tz tz)
    | Baking_rewards, Debited tz, Block_application ->
        Some (Block_rewards, get_tz tz)
    | Baking_bonuses, Debited tz, Block_application ->
        Some (Block_bonuses, get_tz tz)
    | Attesting_rewards, Debited tz, Block_application ->
        Some (Attestation_rewards, get_tz tz)
    | Dal_attesting_rewards, Debited tz, Block_application ->
        Some (Dal_attestation_rewards, get_tz tz)
    (* Block_application - Tracked credited destinations *)
    | Contract contract, Credited tz, Block_application ->
        Some (Contract contract, get_tz tz)
    | Deposits (Baker delegate), Credited tz, Block_application ->
        Some (Baker delegate, get_tz tz)
    | ( Deposits (Single_staker {staker; delegate}),
        Credited tz,
        Block_application ) ->
        Some (Staker (staker, delegate), get_tz tz)
    | ( Deposits (Shared_between_stakers {delegate}),
        Credited tz,
        Block_application ) ->
        Some (Shared delegate, get_tz tz)
    | Deposits (Baker_edge delegate), Credited tz, Block_application ->
        Some (Edge delegate, get_tz tz)
    | Lost_attesting_rewards (addr, _, _), Credited tz, Block_application ->
        Some (Lost_attestation addr, get_tz tz)
    | Lost_dal_attesting_rewards addr, Credited tz, Block_application ->
        Some (Lost_dal_attestation addr, get_tz tz)
    (* Block_application - Reverse direction of tracked sources (not relevant) *)
    | Block_fees, Credited _, Block_application
    | Baking_rewards, Credited _, Block_application
    | Baking_bonuses, Credited _, Block_application
    | Attesting_rewards, Credited _, Block_application
    | Dal_attesting_rewards, Credited _, Block_application ->
        None
    (* Block_application - Reverse direction of tracked destinations (not relevant) *)
    | Contract _, Debited _, Block_application
    | Deposits _, Debited _, Block_application
    | Lost_attesting_rewards _, Debited _, Block_application
    | Lost_dal_attesting_rewards _, Debited _, Block_application ->
        None
    (* Block_application - Non-tracked tez balance types *)
    | Nonce_revelation_rewards, _, Block_application
    | Storage_fees, _, Block_application
    | Double_signing_punishments, _, Block_application
    | Liquidity_baking_subsidies, _, Block_application
    | Burned, _, Block_application
    | Bootstrap, _, Block_application
    | Invoice, _, Block_application
    | Initial_commitments, _, Block_application
    | Minted, _, Block_application
    | Sc_rollup_refutation_punishments, _, Block_application
    | Sc_rollup_refutation_rewards, _, Block_application ->
        None
    | Commitments _, _, Block_application
    | Unstaked_deposits _, _, Block_application
    | Frozen_bonds _, _, Block_application ->
        None
    (* Block_application - Staking pseudotokens (not tez, not tracked) *)
    | Staking_delegator_numerator _, _, Block_application
    | Staking_delegate_denominator _, _, Block_application ->
        None
    (* Other origins - not tracked *)
    | _, _, (Protocol_migration | Subsidy | Simulation | Delayed_operation _) ->
        None

  let category_of_update = function
    | Block_fees -> Some Data.Balance_update.Block_fees
    | Block_rewards -> Some Data.Balance_update.Baking_rewards
    | Block_bonuses -> Some Data.Balance_update.Baking_bonuses
    | Attestation_rewards -> Some Data.Balance_update.Attestation_rewards
    | Dal_attestation_rewards ->
        Some Data.Balance_update.Dal_attestation_rewards
    | Contract _ | Baker _ | Staker _ | Shared _ | Edge _ | Lost_attestation _
    | Lost_dal_attestation _ ->
        None

  let destination_of_update = function
    | Contract (Implicit address) ->
        Some
          ( Tezos_crypto.Signature.Of_V2.public_key_hash address,
            Data.Balance_update.Contract )
    | Contract (Originated _) -> None
    | Baker delegate ->
        Some
          ( Tezos_crypto.Signature.Of_V2.public_key_hash delegate,
            Data.Balance_update.Baker_own_stake )
    | Staker (Implicit pkh, _delegate) ->
        Some
          ( Tezos_crypto.Signature.Of_V2.public_key_hash pkh,
            Data.Balance_update.Staker )
    | Staker (Originated _, _) -> None
    | Shared delegate ->
        Some
          ( Tezos_crypto.Signature.Of_V2.public_key_hash delegate,
            Data.Balance_update.Delegate )
    | Edge delegate ->
        Some
          ( Tezos_crypto.Signature.Of_V2.public_key_hash delegate,
            Data.Balance_update.Baker_edge )
    | Lost_attestation addr ->
        Some
          ( Tezos_crypto.Signature.Of_V2.public_key_hash addr,
            Data.Balance_update.Lost )
    | Lost_dal_attestation addr ->
        Some
          ( Tezos_crypto.Signature.Of_V2.public_key_hash addr,
            Data.Balance_update.Lost )
    | Block_fees | Block_rewards | Block_bonuses | Attestation_rewards
    | Dal_attestation_rewards ->
        None

  let get_balance_updates
      (balance_updates :
        Protocol.Alpha_context.Receipt.balance_update_item trace) =
    let rec fold acc = function
      | [] -> acc
      | [_] ->
          acc (* balance update should have a credited and debited operations *)
      | b1 :: (b2 :: _ as rest) -> (
          match (get_balance_update b1, get_balance_update b2) with
          | Some (source, debited), Some (dest, credited) -> (
              match (category_of_update source, destination_of_update dest) with
              | Some category, Some (address, result) when debited = credited ->
                  let acc =
                    {
                      Data.Balance_update.address;
                      category;
                      result;
                      value = credited;
                    }
                    :: acc
                  in
                  fold acc rest
              | Some _, Some _ | Some _, None | None, Some _ | None, None ->
                  fold acc rest)
          | Some _, None | None, Some _ | None, None -> fold acc rest)
    in
    fold [] balance_updates

  let get_balance_updates cctxt level =
    let* header =
      Block_services.header ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    let* metadata =
      Block_services.metadata ~chain:cctxt#chain ~block:(`Level level) cctxt ()
    in
    let balance_updates =
      get_balance_updates metadata.protocol_data.balance_updates
    in
    let cycle =
      Tezos_protocol_024_PtTALLiN.Protocol.Alpha_context.Cycle.to_int32
        metadata.protocol_data.level_info.cycle
    in
    Format.eprintf
      "%a@."
      (Format.pp_print_list
         ~pp_sep:Format.pp_print_newline
         Data.Balance_update.pp_balance_update)
      balance_updates ;
    return
      ( header.shell.level,
        cycle,
        header.hash,
        header.shell.timestamp,
        balance_updates )
end

module M = General_archiver.Define (Services)
