(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [get ctxt misbehaviour denounced] returns the percentage that
    needs to be applied for the given [misbehaviour].

    [denounced] is the list of delegates that have been denounced
    together for the same level, round, and {!Misbehaviour_repr.kind}
    as the [misbehaviour]. The amount slashed increases quadratically
    as the number of attesting slots of denounced delegates
    increases. The maximum slashing value [max_slashing_per_block] is
    reached when that number of slots reaches
    [max_slashing_threshold].
*)
val get :
  Raw_context.t ->
  Misbehaviour_repr.t ->
  Signature.public_key_hash list ->
  (Raw_context.t * Percentage.t) tzresult Lwt.t

module Internal_for_tests : sig
  val for_double_attestation :
    Raw_context.t ->
    committee_size:int64 ->
    int64 Signature.Public_key_hash.Map.t ->
    Signature.Public_key_hash.t list ->
    Percentage.t
end
