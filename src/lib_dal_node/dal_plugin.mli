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

type operation_application_result =
  | Succeeded
      (** The associated operation is included in a block and successfully
          applied. *)
  | Failed
      (** The associated operation is included in a block but its application was
          not successful (failed, backtracked or skipped). *)

type slot_index = int

(** Information extracted from DAL slots headers operations included in L1
    blocks. Each slot header is made of an L1 level for which it is published,
    the slot's index and commitment. *)
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

  (** [block_info ?chain ?block ~operations_metadata ctxt] returns the
      information of the [block] in [ctxt] for the given [chain]. Operations'
      metadata are included or skipped depending on the value of
      [operations_metadata]. This is a wrapper on top of
      {!Protocol_client_context.Alpha_block_services.info}.  *)
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

  (** For a given block, returns for each included attestation, as a
      list, its Tenderbake slot, its attester if available in the
      operation receipt, its [attestation] operation and, if it
      exists, its [dal_attestation] to be passed to the [is_attested]
      function. *)
  val get_attestations :
    block_level:int32 ->
    Tezos_rpc__RPC_context.generic ->
    (tb_slot
    * Signature.public_key_hash option
    * attestation_operation
    * dal_attestation option)
    list
    tzresult
    Lwt.t

  (** [get_committee ctxt ~level] retrieves the DAL committee at [level] from L1 as a
      map that associates to the public key hash [pkh] of the member of
      the committee its assigned shard indexes. *)
  val get_committee :
    Tezos_rpc.Context.generic ->
    level:int32 ->
    int list Signature.Public_key_hash.Map.t tzresult Lwt.t

  (** [dal_attestation block_info] returns the metadata of the given
      [block_info] as an abstract value of type [dal_attestation] to be passed
      to the [is_attested] function.

      Fails with [Cannot_read_block_metadata] if [block_info]'s metadata are
      stripped.  *)
  val dal_attestation : block_info -> dal_attestation tzresult

  (** [is_attested dal_attestation index] returns [true] if [index]
      is one of the [dal_attestation] and [false] otherwise.  *)
  val is_attested : dal_attestation -> slot_index -> bool

  (** [number_of_attested_slots] returns the number of slots attested in the [dal_attestation]. *)
  val number_of_attested_slots : dal_attestation -> int

  (** [get_round fitness] returns the block round contained in [fitness]. *)
  val get_round : Fitness.t -> int32 tzresult

  (** [block_shell_header block_info] returns the shell header of the block
      whose information are given . *)
  val block_shell_header : block_info -> Block_header.shell_header

  (** Call this function to inject an entrapment evidence to the
      corresponding L1 node. *)
  val inject_entrapment_evidence :
    Tezos_rpc.Context.generic ->
    attested_level:Int32.t ->
    attestation_operation ->
    slot_index:slot_index ->
    shard:Cryptobox.shard ->
    proof:Cryptobox.shard_proof ->
    tb_slot:tb_slot ->
    unit tzresult Lwt.t

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

    (*
      This function mimics what the protocol does in
      {!Dal_slot_storage.finalize_pending_slot_headers}. Given a block_info at
      some level L, an RPC context, the DAL constants for level L, and for level
      L - attestation_lag - 1, the this function computes the cells produced by the
      DAL skip list during the level L using:

       - The information telling which slot headers were waiting for attestation
       at level [L - attestation_lag];

       - The bitset of attested slots at level [L] in the block's metadata.

      It is assumed that at level L the DAL is enabled.

      The ordering of the elements in the returned list is not relevant.
    *)
    val cells_of_level :
      attested_level:int32 ->
      Tezos_rpc.Context.generic ->
      dal_constants:Tezos_dal_node_services.Types.proto_parameters ->
      pred_publication_level_dal_constants:
        Tezos_dal_node_services.Types.proto_parameters tzresult Lwt.t Lazy.t ->
      (hash * cell) list tzresult Lwt.t
  end

  module RPC : sig
    (** RPCs directory of the protocol-related part of the DAL node. *)
    val directory :
      Dal_store_sqlite3.Skip_list_cells.t -> unit Tezos_rpc.Directory.t
  end
end

val register : (module T) -> unit

val get : Protocol_hash.Table.key -> (module T) option
