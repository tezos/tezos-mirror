(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module offers transformation tools from protocol-independent types to
    protocol specific ones. In this particular instance, the agnostic baker CLI
    arguments are defined in a single place, outside the protocol code, and then
    they are mapped, depending on the protocol context.
*)

open Protocol.Alpha_context

(** [parse_configuration] simply transforms a configuration record type into a
    tuple containing the same values. *)
val parse_configuration :
  Configuration.t ->
  string option
  * bool
  * string option
  * Tez.t
  * Q.t
  * Q.t
  * int option
  * bool
  * Per_block_votes.per_block_vote option
  * Per_block_votes.per_block_vote option
  * string option
  * Baking_configuration.Operations_source.t option
  * Uri.t option
  * bool
  * Baking_configuration.state_recorder_config
  * Q.t option
  * Q.t option

(** [parse_baking_mode baking_mode] maps a optional value to a [Local] or [Remote]
    baking mode option. *)
val parse_baking_mode : string option -> Baking_commands.baking_mode

(** [parse_sources sources] maps all public key hashes to the version expected
    by the protocol-specific baking main running function. *)
val parse_sources :
  Tezos_base.TzPervasives.Signature.public_key_hash list ->
  (Tezos_base.TzPervasives.Signature.V1.public_key_hash list, tztrace) result
  Lwt.t
