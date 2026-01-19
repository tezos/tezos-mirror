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

type attestation_lag = int

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

  (* The DAL content an attester includes in its attestation operation. *)
  type dal_attestations

  (* The slot availability information from a block's metadata. *)
  type slot_availability

  type attestation_operation

  type tb_slot

  (** [tb_slot_to_int tb_slot] returns the integer representation of a
      [tb_slot]. *)
  val tb_slot_to_int : tb_slot -> int

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

  (** For a given block, returns for each included attestation, as a list, its
      Tenderbake slot, its [attestation] operation and, if it exists, its
      [dal_attestation] to be passed to the [is_attested] function. *)
  val get_attestations :
    block_level:int32 ->
    Tezos_rpc__RPC_context.generic ->
    (tb_slot * attestation_operation * dal_attestations option) list tzresult
    Lwt.t

  (** [get_committees ctxt ~level] retrieves the DAL and Tenderbake attestation
      committees at [level] from L1 as a map that associates to the public key
      hash [pkh] of the member of the committee its assigned shard indexes and
      attestation slot. Retrieving the attestation slot is particularly useful
      to associate attestations to their senders without relying on a delegate
      pkh.*)
  val get_committees :
    Tezos_rpc.Context.generic ->
    level:int32 ->
    (int list * int) Signature.Public_key_hash.Map.t tzresult Lwt.t

  (** [slot_availability block_info] returns the metadata of the given
      [block_info] as an abstract value of type [slot_availability] to be passed
      to the [is_protocol_attested] function.

      Fails with [Cannot_read_block_metadata] if [block_info]'s metadata are
      stripped.  *)
  val slot_availability : block_info -> slot_availability tzresult

  (** [is_baker_attested dal_attestations ~number_of_slots ~number_of_lags
      ~lag_index slot_index] returns [true] if [slot_index] is set in the bitset
      of the [dal_attestation] at the given [lag_index] and [false] otherwise.  *)
  val is_baker_attested :
    dal_attestations ->
    number_of_slots:int ->
    number_of_lags:int ->
    lag_index:int ->
    slot_index ->
    bool

  (** [is_protocol_attested slot_availability ~number_of_slots ~number_of_lags
      ~lag_index slot_index] returns [true] if [slot_index] is one of the
      attested slots in [slot_availability] at the given [lag_index] and [false]
      otherwise. *)
  val is_protocol_attested :
    slot_availability ->
    number_of_slots:int ->
    number_of_lags:int ->
    lag_index:int ->
    slot_index ->
    bool

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

  (** This function constructs and injects the L1 publication operation.
      It is meant to be used in a test context, as it requires the explicit
      provision of the secret key, which is not desirable in production. *)
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

    (** [back_pointer cell ~index:i] returns [Ok (Some hash)] if [hash] is the
        [i]-th back pointer of [cell]. Returns [Ok None] if the cell contains
        fewer than [i + 1] back pointers.

        [back_pointer cell ~index:0] returns the {!hash} of the previous cell,
        if any.

        The function returns [Error ()] for older protocols that don't expose
        the needed function to access cells' back pointers. *)
    val back_pointer : cell -> index:int -> (hash option, unit) result

    (* Returns the DAL skip list cells produced at the given attested level.
       Each cell is associated with its hash and slot index. There are
       [number_of_slots] cells per level. *)
    val cells_of_level :
      attested_level:int32 ->
      Tezos_rpc.Context.generic ->
      dal_constants:Tezos_dal_node_services.Types.proto_parameters ->
      pred_publication_level_dal_constants:
        Tezos_dal_node_services.Types.proto_parameters tzresult Lwt.t Lazy.t ->
      (hash * cell * slot_index * attestation_lag) list tzresult Lwt.t

    (** Extracts and returns the slot header of the given cell if it was
        published to L1. *)
    val slot_header_of_cell : cell -> slot_header option

    (** Returns [Some (`Attested | `Unattested | `Unpublished)] when the protocol
       attestation status of the given cell is attested, unattested, or
       unpublished.

       The function returns [None] for older protocols that do not expose
       the necessary information to access the status. *)
    val proto_attestation_status :
      cell -> [`Attested of attestation_lag | `Unattested | `Unpublished] option
  end

  module RPC : sig
    (** RPCs directory of the protocol-related part of the DAL node. *)
    val directory :
      Dal_store_sqlite3.Skip_list_cells.t -> unit Tezos_rpc.Directory.t
  end
end

val register : (module T) -> unit

val get : Protocol_hash.Table.key -> (module T) option
