(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type operation_application_result = Succeeded | Failed

type slot_index = int

type slot_header = {
  published_level : int32;
  slot_index : slot_index;
  commitment : Tezos_crypto_dal.Cryptobox.Verifier.commitment;
}

type proto_parameters = {
  feature_enable : bool;
  incentives_enable : bool;
  number_of_slots : int;
  attestation_lag : int;
  attestation_threshold : int;
  cryptobox_parameters : Tezos_crypto_dal.Cryptobox.Verifier.parameters;
  sc_rollup_challenge_window_in_blocks : int;
  commitment_period_in_blocks : int;
  dal_attested_slots_validity_lag : int;
  blocks_per_cycle : int32;
}

let proto_parameters_encoding : proto_parameters Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           feature_enable;
           incentives_enable;
           number_of_slots;
           attestation_lag;
           attestation_threshold;
           cryptobox_parameters;
           sc_rollup_challenge_window_in_blocks;
           commitment_period_in_blocks;
           dal_attested_slots_validity_lag;
           blocks_per_cycle;
         } ->
      ( feature_enable,
        incentives_enable,
        number_of_slots,
        attestation_lag,
        attestation_threshold,
        cryptobox_parameters,
        sc_rollup_challenge_window_in_blocks,
        commitment_period_in_blocks,
        dal_attested_slots_validity_lag,
        blocks_per_cycle ))
    (fun ( feature_enable,
           incentives_enable,
           number_of_slots,
           attestation_lag,
           attestation_threshold,
           cryptobox_parameters,
           sc_rollup_challenge_window_in_blocks,
           commitment_period_in_blocks,
           dal_attested_slots_validity_lag,
           blocks_per_cycle ) ->
      {
        feature_enable;
        incentives_enable;
        number_of_slots;
        attestation_lag;
        attestation_threshold;
        cryptobox_parameters;
        sc_rollup_challenge_window_in_blocks;
        commitment_period_in_blocks;
        dal_attested_slots_validity_lag;
        blocks_per_cycle;
      })
    (obj10
       (req "feature_enable" bool)
       (req "incentives_enable" bool)
       (req "number_of_slots" int31)
       (req "attestation_lag" int31)
       (req "attestation_threshold" int31)
       (req
          "cryptobox_parameters"
          Tezos_crypto_dal.Cryptobox.Verifier.parameters_encoding)
       (req "sc_rollup_challenge_window_in_blocks" int31)
       (req "commitment_period_in_blocks" int31)
       (req "dal_attested_slots_validity_lag" int31)
       (req "blocks_per_cycle" int32))

module type T = sig
  module Proto : Registered_protocol.T

  type block_info

  type attested_indices

  val block_info :
    ?chain:Tezos_shell_services.Block_services.chain ->
    ?block:Tezos_shell_services.Block_services.block ->
    metadata:[`Always | `Never] ->
    Tezos_rpc.Context.generic ->
    block_info tzresult Lwt.t

  val get_constants :
    Tezos_shell_services.Chain_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Tezos_rpc.Context.generic ->
    proto_parameters tzresult Lwt.t

  val get_published_slot_headers :
    block_info ->
    (slot_header * operation_application_result) list tzresult Lwt.t

  val get_committee :
    Tezos_rpc.Context.generic ->
    level:int32 ->
    int list Tezos_crypto.Signature.Public_key_hash.Map.t tzresult Lwt.t

  val attested_slot_headers : block_info -> attested_indices tzresult

  val is_attested : attested_indices -> slot_index -> bool

  val get_round : Fitness.t -> int32 tzresult

  val block_shell_header : block_info -> Block_header.shell_header

  (* Section of helpers for Skip lists *)

  module Skip_list : sig
    type cell

    type hash

    val cell_encoding : cell Data_encoding.t

    val hash_encoding : hash Data_encoding.t

    val cell_equal : cell -> cell -> bool

    val hash_equal : hash -> hash -> bool

    val cell_hash : cell -> hash

    val cells_of_level :
      block_info ->
      Tezos_rpc.Context.generic ->
      dal_constants:proto_parameters ->
      pred_publication_level_dal_constants:
        proto_parameters tzresult Lwt.t Lazy.t ->
      (hash * cell) list tzresult Lwt.t
  end

  module RPC : sig
    val directory :
      [< `KVS of Skip_list_cells_store.t | `SQLite3 of Dal_store_sqlite3.t] ->
      unit Tezos_rpc.Directory.t
  end
end

let table : (module T) Protocol_hash.Table.t = Protocol_hash.Table.create 5

let register (module Plugin : T) =
  assert (not (Protocol_hash.Table.mem table Plugin.Proto.hash)) ;
  Protocol_hash.Table.add table Plugin.Proto.hash (module Plugin)

let get hash = Protocol_hash.Table.find table hash
