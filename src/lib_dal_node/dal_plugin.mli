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

val proto_parameters_encoding : proto_parameters Data_encoding.t

module type T = sig
  module Proto : Registered_protocol.T

  type block_info

  type attested_indices

  (** [block_info ?chain ?block ~metadata ctxt] returns the information of the
      [block] in [ctxt] for the given [chain]. Block's metadata are included or
      skipped depending on the value of [metadata]. This is a wrapper on top of
      {!Protocol_client_context.Alpha_block_services.info}.  *)
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

  (** [get_committee ctxt ~level] retrieves the DAL committee at [level] from L1 as a
      map that associates to the public key hash [pkh] of the member of
      the committee its assigned shard indexes. *)
  val get_committee :
    Tezos_rpc.Context.generic ->
    level:int32 ->
    int list Tezos_crypto.Signature.Public_key_hash.Map.t tzresult Lwt.t

  (** [attested_slot_headers block_info] reads the metadata of the
      given [block_info] and constructs the list of attested slot
      indices as an abstract value of type [attested_indices] to be
      passed to the [is_attested] function.

      Fails with [Cannot_read_block_metadata] if [block_info]'s metadata are
      stripped.  *)
  val attested_slot_headers : block_info -> attested_indices tzresult

  (** [is_attested attested_indices index] returns [true] if [index]
      is one of the [attested_indices] and [false] otherwise.  *)
  val is_attested : attested_indices -> slot_index -> bool

  (** [get_round fitness] returns the block round contained in [fitness]. *)
  val get_round : Fitness.t -> int32 tzresult

  (** [block_shell_header block_info] returns the shell header of the block
      whose information are given . *)
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
      block_info ->
      Tezos_rpc.Context.generic ->
      dal_constants:proto_parameters ->
      pred_publication_level_dal_constants:
        proto_parameters tzresult Lwt.t Lazy.t ->
      (hash * cell) list tzresult Lwt.t
  end

  module RPC : sig
    (** RPCs directory of the protocol-related part of the DAL node. *)
    val directory : Skip_list_cells_store.t -> unit Tezos_rpc.Directory.t
  end
end

val register : (module T) -> unit

val get : Protocol_hash.Table.key -> (module T) option
