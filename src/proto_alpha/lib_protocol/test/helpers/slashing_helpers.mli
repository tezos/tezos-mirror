(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers related to denunciations and slashing. *)

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
    Tezos_raw_protocol_alpha.Misbehaviour_repr.t ->
    'kind Protocol.Alpha_context.Kind.consensus Protocol.Alpha_context.operation ->
    unit tzresult Lwt.t
end
