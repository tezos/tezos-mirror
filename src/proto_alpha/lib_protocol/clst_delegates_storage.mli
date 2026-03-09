(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles registration of delegates in the storage and
    their pending parameters activation. *)

(** [get_delegate_parameters ctxt delegate] returns the active CLST
    parameters of [delegate]. Returns None if the delegate has no
    active parameters. *)
val get_delegate_parameters :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Clst_delegates_parameters_repr.t option tzresult Lwt.t

(** [get_pending_parameters ctxt delegate] returns the list of cycles at which
    newly registered parameters will be activated. *)
val get_pending_parameters :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  (Cycle_repr.t * Clst_delegates_parameters_repr.t) list tzresult Lwt.t

(** [register_pending_parameters ctxt delegate parameters] updates [delegate]'s
    [parameters] to be activated after
    {!Constant_storage.consensus_rights_delay} cycles. These
    parameters are not activated yet, but serve for future baking rights. *)
val register_pending_parameters :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Clst_delegates_parameters_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [activate_parameters ctxt ~new_cycle] activates the clst pending delegate
    parameters at the beginning of cycle [new_cycle]. This function iterates
    over all delegates registered with the CLST. *)
val activate_parameters :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t Lwt.t
