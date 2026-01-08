(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Here are defined various roles that agents (like L1 nodes, DAL nodes,
    Bakers etc.) can assume in the tezt-cloud test scenarios.

    These roles are used to systematically name and distinguish
    agents and their responsibilities across different configurations.
*)

type t =
  | Bootstrap
      (** The main bootstrap node and client, typically the entry point of the network. *)
  | Baker of int
      (** A node responsible for block production, identified by index. *)
  | Producer of int  (** A DAL slot producer node, identified by index. *)
  | Observer of [`Indexes of int list | `Pkh of string]
      (** A DAL observer node, either indexed or associated with a public key hash. *)
  | Archiver of [`Indexes of int list]
      (** A DAL archiver node, identified by indexes. *)
  | Reverse_proxy
      (** A reverse proxy between rollup nodes and multiple DAL nodes. *)
  | Etherlink_operator  (** The main Etherlink rollup operator node. *)
  | Etherlink_dal_operator
      (** A DAL node running alongside the Etherlink operator. *)
  | Etherlink_dal_observer of {slot_index : int}
      (** An Etherlink DAL observer responsible for a specific slot index. *)
  | Etherlink_producer of int  (** A DAL slot producer used by Etherlink. *)
  | Echo_rollup_operator of int  (** A rollup operator for the Echo rollup. *)
  | Echo_rollup_dal_observer of {operator : int; slot_index : int}
      (** A DAL observer node for the Echo rollup, indexed by operator and slot. *)
  | Stresstest of int

val rex_bootstrap : rex

val rex_baker_index : rex

val rex_producer_index : rex

val rex_observer_indexes : rex

val rex_observer_pkh : rex

val rex_archiver_indexes : rex

val rex_reverse_proxy : rex

val rex_etherlink_operator : rex

val rex_etherlink_dal_operator : rex

val rex_etherlink_dal_observer_index : rex

val rex_etherlink_producer_index : rex

val rex_echo_rollup_operator_index : rex

val rex_echo_rollup_dal_observer_index : int -> rex

val rex_stresstest_index : rex

(** [name_of t] returns the standard name associated with a given [t].
    Used for consistent naming of VMs, logs and artifacts.

    The name matches the regular expressions defined above, where:
    - if [rex] ends with [_index]: [(\\d+)] has been replaced with
      the integer payload of the [t] value.
    - if [rex] ends with [_pkh]: [([a-zA-Z0-9]+)] has been replaced with
      the first 8 bytes of the pkh payload of the [t] value.
    - else: this is a constant string. *)
val name_of : t -> string

(** [daemon] stands for a binary that is running onto an agent. A [daemon] is
    identified by a standard name that is used for consistent namings. *)
type daemon =
  | Baker_l1_node of int
  | Baker_dal_node of int
  | Producer_l1_node of int
  | Producer_dal_node of int
  | Observer_l1_node of int
  | Observer_dal_node of int
  | Archiver_l1_node of int
  | Archiver_dal_node of int
  | Echo_rollup_node of string
  | Etherlink_sc_rollup_node of string
  | Etherlink_evm_node of string
  | Etherlink_producer_node of string

(** [name_of_daemon] returns the standard name associated with a given [daemon].
    Used for consistent naming of VMs, logs and artifacts. *)
val name_of_daemon : daemon -> string

module Logs : sig
  (** [scp_logs ?log_dir_name ~destination_root ~daemon_name agent] uses scp to
      copy the `daily_logs` ([log_dir_name] can be set to override the default
      "daily_logs" path) directory from the VM hosting the [agent]'s actor
      given by [~daemon_name] into
      [~destination_root/<agent-name>/~daemon_name/daily_logs].

      If the agent has no SSH runner or the copying process fails, the function is
      a no-op (with a corresponding warning). Any missing directory is automatically
      created. *)
  val scp_logs :
    ?log_dir_name:string ->
    destination_root:string ->
    daemon_name:string ->
    Agent.t ->
    unit Lwt.t

  val scp_profiling :
    destination_root:string -> daemon_name:string -> Agent.t -> unit Lwt.t
end
