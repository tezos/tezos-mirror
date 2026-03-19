(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [save_parameters ctxt constants_previous ~next_protocol_activation] is called
    when a new protocol is activated, that is at the end of the last block of its
    previous protocol. Therefore [next_protocol_activation] is the level of the
    last block of the previous protocol, alongside the protocol constants used
    for this protocol: [constant_previous].

    The activation level and the parameters are saved in the context. Note that
    it transforms {!Constants_parametric_previous_repr.dal} to
    {!Constants_parametric_repr.dal}, therefore it assumes that it can be
    transformed at migration time. If for a given protocol it's not possible,
    retrocompatible encoding needs to be introduced. *)
val save_parameters :
  Raw_context.t ->
  Constants_parametric_previous_repr.dal ->
  next_protocol_activation:Raw_level_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [parameters ctxt level] returns the constants used for a given [level]. When
    [level] is a migration level, it returns the constants of the protocol that
    ends at that level. If the level is greater than any protocol migration
    level, it returns the current protocol constants. *)
val parameters :
  Raw_context.t ->
  Raw_level_repr.t ->
  Constants_parametric_repr.dal tzresult Lwt.t
