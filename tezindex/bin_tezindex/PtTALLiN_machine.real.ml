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

  let get_balance_update
      (Balance_update_item (balance, balance_update, origin) :
        Protocol.Alpha_context.Receipt.balance_update_item) =
    let get_tz tz = Protocol.Alpha_context.Tez.to_mutez tz in
    match (balance, balance_update, origin) with
    (* Block application *)
    (* Debited *)
    | Block_fees, Debited tz, Block_application -> Some (Block_fees, get_tz tz)
    | Baking_rewards, Debited tz, Block_application ->
        Some (Block_rewards, get_tz tz)
    | Baking_bonuses, Debited tz, Block_application ->
        Some (Block_bonuses, get_tz tz)
    | Attesting_rewards, Debited tz, Block_application ->
        Some (Attestation_rewards, get_tz tz)
    | Dal_attesting_rewards, Debited tz, Block_application ->
        Some (Dal_attestation_rewards, get_tz tz)
    (* Credited *)
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
    | Lost_attesting_rewards (_addr, _b, _b'), Credited _tz, Block_application
      ->
        None (* TODO *)
    | Lost_dal_attesting_rewards _addr, Credited _tz, Block_application ->
        None (* TODO *)
    | _, _, Block_application -> None (* TODO *)
    (* Other Origin *)
    | _, _, (Protocol_migration | Subsidy | Simulation | Delayed_operation _) ->
        None (* TODO *)

  let get_balance_updates
      (balance_updates :
        Protocol.Alpha_context.Receipt.balance_update_item trace) =
    let rec fold acc = function
      | [] -> acc
      | [_] ->
          acc (* balance update should have a credited and debited operations *)
      | b1 :: (b2 :: _ as rest) -> (
          match (get_balance_update b1, get_balance_update b2) with
          | ( Some (Block_fees, debited),
              Some (Contract (Implicit address), credited) )
            when debited = credited ->
              let acc =
                {
                  Data.Balance_update.address =
                    Tezos_crypto.Signature.Of_V2.public_key_hash address;
                  category = Block_fees;
                  result = Contract;
                  value = credited;
                }
                :: acc
              in
              fold acc rest
          | None, None -> fold acc rest
          | _, _ -> fold acc rest)
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
    Format.eprintf
      "%a@."
      (Format.pp_print_list Data.Balance_update.pp_balance_update)
      balance_updates ;
    return
      (header.shell.level, header.hash, header.shell.timestamp, balance_updates)
end

module M = General_archiver.Define (Services)
