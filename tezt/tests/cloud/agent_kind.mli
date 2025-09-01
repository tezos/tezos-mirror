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
  | Observer of [`Index of int | `Pkh of string]
      (** A DAL observer node, either indexed or associated with a public key hash. *)
  | Reverse_proxy
      (** A reverse proxy between rollup nodes and multiple DAL nodes. *)
  | Etherlink_operator  (** The main Etherlink rollup operator node. *)
  | Etherlink_dal_operator
      (** A DAL node running alongside the Etherlink operator. *)
  | Etherlink_dal_observer of {slot_index : int}
      (** An Etherlink DAL observer responsible for a specific slot index. *)
  | Etherlink_producer of int  (** A DAL slot producer used by Etherlink. *)
  | Echo_rollup_operator  (** The main rollup operator for the Echo rollup. *)
  | Echo_rollup_dal_observer of {slot_index : int}
      (** A DAL observer node for the Echo rollup, indexed by slot. *)
  | Stresstest of int

(** [name_of agent] returns the standard name associated with a given [agent].
    Used for consistent naming of VMs, logs and artifacts. *)
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
  | Echo_rollup_node of string
  | Etherlink_sc_rollup_node of string
  | Etherlink_evm_node of string
  | Etherlink_producer_node of string

(** [name_of_daemon] returns the standard name associated with a given [daemon].
    Used for consistent naming of VMs, logs and artifacts. *)
val name_of_daemon : daemon -> string

module Logs : sig
  (** [local_path path] ensures that the path (as a list of subdirectories) is a
      valid directory, creates it otherwise, then returns the path. If the first
      directory starts with `/`, it will start from the root of the
      filesystem. *)
  val local_path : string list -> string

  (** [scp_logs ~destination_root ~daemon_name agent] uses scp to copy the
      `daily_logs` directory from the VM hosting the [agent]'s actor given by
      [~daemon_name] into [~destination_root/<agent-name>/~daemon_name/daily_logs].

      If the agent has no SSH runner or the copying process fails, the function is
      a no-op (with a corresponding warning). Any missing directory is automatically
      created. *)
  val scp_logs :
    destination_root:string -> daemon_name:string -> Agent.t -> unit Lwt.t

  val scp_profiling :
    destination_root:string -> daemon_name:string -> Agent.t -> unit Lwt.t
end
