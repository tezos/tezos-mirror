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

type attestation_lag = int

type slot_header = {
  published_level : int32;
  slot_index : slot_index;
  commitment : Cryptobox.Verifier.commitment;
}

module type T = sig
  module Proto : Registered_protocol.T

  type block_info

  type dal_attestation

  type attestation_operation

  type tb_slot

  val tb_slot_to_int : tb_slot -> int

  val block_info :
    ?chain:Tezos_shell_services.Block_services.chain ->
    ?block:Tezos_shell_services.Block_services.block ->
    operations_metadata:[`Always | `Never] ->
    Tezos_rpc.Context.generic ->
    block_info tzresult Lwt.t

  val get_constants :
    Tezos_shell_services.Chain_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Tezos_rpc.Context.generic ->
    Tezos_dal_node_services.Types.proto_parameters tzresult Lwt.t

  val get_published_slot_headers :
    block_level:int32 ->
    Tezos_rpc__RPC_context.generic ->
    slot_header list tzresult Lwt.t

  val get_attestations :
    block_level:int32 ->
    Tezos_rpc__RPC_context.generic ->
    (tb_slot * attestation_operation * dal_attestation option) list tzresult
    Lwt.t

  val get_committees :
    Tezos_rpc.Context.generic ->
    level:int32 ->
    (int list * int) Signature.Public_key_hash.Map.t tzresult Lwt.t

  val dal_attestation : block_info -> dal_attestation tzresult

  val is_attested : dal_attestation -> slot_index -> bool

  val number_of_attested_slots : dal_attestation -> int

  val get_round : Fitness.t -> int32 tzresult

  val block_shell_header : block_info -> Block_header.shell_header

  val inject_entrapment_evidence :
    Tezos_rpc.Context.generic ->
    attested_level:Int32.t ->
    attestation_operation ->
    slot_index:slot_index ->
    shard:Cryptobox.shard ->
    proof:Cryptobox.shard_proof ->
    tb_slot:tb_slot ->
    unit tzresult Lwt.t

  val publish :
    Tezos_rpc.Context.generic ->
    block_level:int32 ->
    source:Signature.Public_key_hash.t ->
    slot_index:slot_index ->
    commitment:Tezos_crypto_dal.Cryptobox.commitment ->
    commitment_proof:Tezos_crypto_dal.Cryptobox.commitment_proof ->
    src_sk:Signature.Secret_key.t ->
    unit ->
    Operation_hash.t tzresult Lwt.t

  val is_delegate :
    Tezos_rpc.Context.generic ->
    pkh:Signature.Public_key_hash.t ->
    bool tzresult Lwt.t

  (* Section of helpers for Skip lists *)

  module Skip_list : sig
    type cell

    type hash

    val cell_encoding : cell Data_encoding.t

    val hash_encoding : hash Data_encoding.t

    val cell_equal : cell -> cell -> bool

    val hash_equal : hash -> hash -> bool

    val cell_hash : cell -> hash

    val back_pointer : cell -> index:int -> (hash option, unit) result

    val cells_of_level :
      attested_level:int32 ->
      Tezos_rpc.Context.generic ->
      dal_constants:Tezos_dal_node_services.Types.proto_parameters ->
      pred_publication_level_dal_constants:
        Tezos_dal_node_services.Types.proto_parameters tzresult Lwt.t Lazy.t ->
      (hash * cell * slot_index * attestation_lag) list tzresult Lwt.t

    val slot_header_of_cell : cell -> slot_header option

    val proto_attestation_status :
      cell -> [`Attested of attestation_lag | `Unattested | `Unpublished] option
  end

  module RPC : sig
    val directory :
      Dal_store_sqlite3.Skip_list_cells.t -> unit Tezos_rpc.Directory.t
  end
end

let table : (module T) Protocol_hash.Table.t = Protocol_hash.Table.create 5

let register (module Plugin : T) =
  assert (not (Protocol_hash.Table.mem table Plugin.Proto.hash)) ;
  Protocol_hash.Table.add table Plugin.Proto.hash (module Plugin)

let get hash = Protocol_hash.Table.find table hash
