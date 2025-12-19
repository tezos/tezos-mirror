(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides functionality related to protocol plugins. *)

(** Values of these type represent protocol plugins supported by the DAL node. A
    protocol plugin is associated with a [first_level] from which it can be
    used, and with the counter [proto_level] that identifies which protocol it
    corresponds to. *)
type t

(** It builds a value of type [t] containing a single plugin, given as
    parameter. *)
val singleton :
  first_level:int32 ->
  proto_level:int ->
  (module Dal_plugin.T) ->
  Types.proto_parameters ->
  t

val to_list : t -> (module Dal_plugin.T) list

(** [current_proto_level plugins] returns the [proto_level] of the most recently
    added protocol plugin. *)
val current_proto_level : t -> int option

val current_proto_parameters : t -> Types.proto_parameters option

type error +=
  | No_plugin_for_level of {level : int32}
  | No_plugin_for_proto of {proto_hash : Protocol_hash.t}

(** [get_plugin_for_level plugins ~level] searches in [plugins] the plugin for
    the given [level] and returns it if found together with the associated
    protocol parameters. Otherwise, it returns the error [No plugin_for_level
    level].

    Recall that, for a migration level L:
    * to retrieve the metadata of the block L, one should use the plugin for the
      old protocol;
    * to retrieve context-related information, one should use the plugin for the
      new protocol.
    This function returns the plugin of [metadata.protocols.next_protocol], so
    it is tailored for the second use case. To get plugin for the first
    use-case, just get the plugin for the predecessor of the target level. *)
val get_plugin_and_parameters_for_level :
  t -> level:int32 -> ((module Dal_plugin.T) * Types.proto_parameters) tzresult

(** [may_add rpc_ctxt plugins ~first_level ~proto_level] may add to [plugins] a
    new plugin for the protocol with level [proto_level] to be used starting
    with the given [first_level] if no suitable plugin is already present in
    [plugin].

    It returns an error if the [Chain_services.Blocks.protocols] RPC fails, or
    if the plugin is not registered, in which case it returns
    [No_plugin_for_proto]. *)
val may_add :
  Rpc_context.t -> t -> first_level:int32 -> proto_level:int -> t tzresult Lwt.t

(** [get_supported_proto_plugins cctxt ~head_level] fetches all the protocol plugins that
    it can and for which it can also fetch the protocol parameters.

    This function is typically called once at startup after fetching the current
    [head_level] from L1.
*)
val get_supported_proto_plugins :
  Rpc_context.t -> head_level:int32 -> t tzresult Lwt.t

(** returns true if an only if the given proto_plugins structure has some
    plugins registered in it. *)
val has_plugins : t -> bool
