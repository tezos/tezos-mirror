(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Returns the pending denunciations list of the given delegate.
    It returns an empty list if none are registered.
  *)
val find :
  Raw_context.t ->
  Signature.public_key_hash ->
  Denunciations_repr.item list tzresult Lwt.t

(** Add a denunciation in the list of the given delegate  *)
val add_denunciation :
  Raw_context.t ->
  misbehaving_delegate:Signature.public_key_hash ->
  Operation_hash.t ->
  rewarded_delegate:Signature.public_key_hash ->
  Misbehaviour_repr.t ->
  Raw_context.t tzresult Lwt.t

(** Set the denunciation list of the given delegate.
    Previously set denunciations would be erased.
*)
val set_denunciations :
  Raw_context.t ->
  Signature.public_key_hash ->
  Denunciations_repr.t ->
  Raw_context.t Lwt.t

(** Tells if the given delegate has some pending denunciations  *)
val has_pending_denunciations :
  Raw_context.t -> Signature.public_key_hash -> bool Lwt.t

(** See {!Storage.Pending_denunciations.fold}  *)
val fold :
  Raw_context.t ->
  order:[`Sorted | `Undefined] ->
  init:'a ->
  f:
    (Signature.public_key_hash ->
    Denunciations_repr.item list ->
    'a ->
    'a Lwt.t) ->
  'a Lwt.t

(** See {!Storage.Pending_denunciations.clear}  *)
val clear : Raw_context.t -> Raw_context.t Lwt.t

module For_RPC : sig
  (** Returns a list of all denunciations paired with the offending delegate pkh. *)
  val pending_denunciations_list :
    Raw_context.t ->
    (Signature.public_key_hash * Denunciations_repr.item) list Lwt.t
end
