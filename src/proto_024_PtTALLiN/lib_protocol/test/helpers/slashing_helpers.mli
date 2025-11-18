(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers related to denunciations and slashing. *)

(** Returns the slashing percentage to apply for the given
    misbehaviour.

    Preconditions:

    - [all_culprits] is the list of all delegates that have been
      denounced for the given misbehaviour.

    - [block_before_slash] is any block from cycle
      [misbehaviour_cycle - consensus_rights_delay] up to the last block
      **before** the slashing gets applied. Indeed, the block that
      applies the slashing also moves on to the next cycle and discards
      the seed for the misbehaviour level rights from the context. *)
val slashing_percentage :
  block_before_slash:Block.t ->
  Protocol.Misbehaviour_repr.t ->
  all_culprits:Signature.public_key_hash list ->
  Protocol.Percentage.t tzresult Lwt.t

(** Helpers related to {!Protocol.Misbehaviour_repr}. *)
module Misbehaviour_repr : sig
  val pp : Format.formatter -> Protocol.Misbehaviour_repr.t -> unit

  (** Builds a misbehaviour object from either of the duplicate
      (pre)attestations that constitute a double (pre)attestating
      event. *)
  val from_duplicate_operation :
    'kind Protocol.Alpha_context.Kind.consensus Protocol.Alpha_context.operation ->
    Protocol.Misbehaviour_repr.t

  (** [check_from_duplicate_operation ~loc misbehaviour duplicate_op]
      asserts that [misbehaviour] correctly describes a double signing
      event involving [duplicate_op]. *)
  val check_from_duplicate_operation :
    loc:string ->
    Tezos_raw_protocol_024_PtTALLiN.Misbehaviour_repr.t ->
    'kind Protocol.Alpha_context.Kind.consensus Protocol.Alpha_context.operation ->
    unit tzresult Lwt.t

  (** Builds a misbehaviour object from either of the duplicate blocks
      that constitute a double baking event. *)
  val from_duplicate_block : Block.t -> Protocol.Misbehaviour_repr.t tzresult
end

(** Helpers about "full denunciations", that is, a denunciation item
    and its culprit. See type [t] of this module. *)
module Full_denunciation : sig
  (** A denunciation item preceded by the culprit's pkh. Indeed, the
      culprit isn't recorded inside the
      {!Protocol.Denunciations_repr.item} because it serves as a key
      in the protocol's storage instead. But we often need both
      together in the tests. *)
  type t = Signature.Public_key_hash.t * Protocol.Denunciations_repr.item

  (** Asserts that both lists contain the same elements.

      These elements may be ordered differently, but must have the
      same multiplicity in both lists. *)
  val check_same_lists_any_order :
    loc:string -> t list -> t list -> unit tzresult Lwt.t

  (** Same as {!slashing_percentage} (top-level function), but takes
      directly a list of full denunciations as an argument.

      The list of full denunciation must contain at least all the
      denunciations for [misbehaviour] known to the chain. Note that it
      may additionally contain denunciations for other misbehaviours,
      which will be ignored.

      See {!slashing_percentage} for the precondition on
      [block_before_slash]. *)
  val slashing_percentage :
    block_before_slash:Block.t ->
    Protocol.Misbehaviour_repr.t ->
    t list ->
    Protocol.Percentage.t tzresult Lwt.t
end

(** Applies all slashes at cycle end in the state *)
val apply_all_slashes_at_cycle_end :
  Protocol.Alpha_context.Cycle.t -> Block.t -> State.t -> State.t tzresult Lwt.t
