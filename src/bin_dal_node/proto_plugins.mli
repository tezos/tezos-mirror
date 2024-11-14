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
  first_level:int32 -> proto_level:int -> (module Dal_plugin.T) -> t

val to_list : t -> (module Dal_plugin.T) list

type error +=
  | No_plugin_for_level of {level : int32}
  | No_plugin_for_proto of {proto_hash : Protocol_hash.t}

(** [get_plugin_for_level plugins ~level] searches in [plugins] the plugin for
    the given [level] and returns it if found. Otherwise, it returns the error
    [No plugin_for_level level].

    Recall that, for a migration level L:
    * to retrieve the metadata of the block L, one should use the plugin for the
      old protocol;
    * to retrieve context-related information, one should use the plugin for the
      new protocol.
    This function returns the plugin of [metadata.protocols.next_protocol], so it is
    tailored for the second use case. To get plugin for the first use-case, just
    get the plugin for the predecessor of the target level. *)
val get_plugin_for_level : t -> level:int32 -> (module Dal_plugin.T) tzresult

(** [may_add rpc_ctxt plugins ~first_level ~proto_level] may add to [plugins] a
    new plugin for the protocol with level [proto_level] to be used starting
    with the given [first_level] if no suitable plugin is already present in
    [plugin].

    It returns an error if the [Chain_services.Blocks.protocols] RPC fails, or
    if the plugin is not registered, in which case it returns
    [No_plugin_for_proto]. *)
val may_add :
  Rpc_context.t -> t -> first_level:int32 -> proto_level:int -> t tzresult Lwt.t

(** [initial_plugins rpc_ctxt ~first_level ~last_level] returns the plugins for
    levels between [first_level] and [last_last]. Note that if migrations have
    happened in this interval, there will be several plugins.

    It returns an error if the [Chain_services.Blocks.protocols] RPC fails, or
    if some plugin is not registered, in which case it returns
    [No_plugin_for_proto]. *)
val initial_plugins :
  Rpc_context.t -> first_level:int32 -> last_level:int32 -> t tzresult Lwt.t

(** [resolve_plugin_for_level rpc_ctxt ~level] returns the plugin for the given
    [level].

    It returns an error if the [Chain_services.Blocks.protocols] RPC fails, or
    if the plugin is not registered, in which case it returns
    [No_plugin_for_proto]. *)
val resolve_plugin_for_level :
  Rpc_context.t -> level:int32 -> (module Dal_plugin.T) tzresult Lwt.t
