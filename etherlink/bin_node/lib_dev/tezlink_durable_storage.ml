(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_types

(** Thin readers on top of [Durable_storage] for the Michelson-runtime
    implicit-account subtree (balance / manager / counter), plus the
    DA-fee mutez helper. The Originated/Implicit dispatch and the
    encoding/decoding live in the resolver of the underlying typed
    constructors. *)

let balance state c =
  Durable_storage.read_or_default ~default:Tez.zero (Tezlink_balance c) state

let balance_z state c =
  let open Lwt_result_syntax in
  let* b = balance state c in
  return @@ Tez.to_mutez_z b

let manager state pkh =
  Durable_storage.read_or_default ~default:None (Tezlink_manager pkh) state

let counter state pkh =
  Durable_storage.read_or_default ~default:None (Tezlink_counter pkh) state

(* Mirrors the kernel's genesis block hash ([TezBlock::genesis_block_hash]). *)
let genesis_block_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9"

(* Number of recent blocks kept in the kernel's live_blocks window
   ([BLOCKS_STORED] in revm-etherlink). *)
let live_blocks_window = Z.of_int 256

(* Whether [branch] is a valid branch for a native operation, mirroring the
   kernel's [is_valid_tez_branch] so the prevalidator rejects exactly what the
   kernel would drop at inclusion: it must be one of the recent blocks in the
   [live_blocks] set, or the genesis hash while the chain is still within its
   first [live_blocks_window] blocks (genesis is not stored in live_blocks). *)
let is_valid_branch state branch =
  let open Lwt_result_syntax in
  let* live = Durable_storage.read_opt (Tezos_live_block branch) state in
  if Option.is_some live then return true
  else if Block_hash.equal branch genesis_block_hash then
    let* block = Durable_storage.read_opt Tezosx_tezos_current_block state in
    let current =
      match block with
      | Some b ->
          let (Ethereum_types.Qty n) = L2_types.block_number b in
          n
      | None -> Z.zero
    in
    return Z.(lt current live_blocks_window)
  else return false

let da_fee_per_byte_mutez state =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty da_fee_per_byte_wei) =
    Durable_storage.read Da_fee_per_byte state
  in
  let*? da_fee_per_byte = Tezos_types.Tez.(of_wei (Wei da_fee_per_byte_wei)) in
  (* DA fee expressed in wei: converting to mutez. *)
  return da_fee_per_byte
