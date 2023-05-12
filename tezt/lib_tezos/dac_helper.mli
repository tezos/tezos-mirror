(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers for configuring DAC scenarios. *)

(* Repr for different kinds of scenarios that tezt tests can run against. *)
module Scenarios : sig
  (* Components for full DAC infrastructure. *)
  type full = {
    protocol : Protocol.t;
    node : Node.t;
    client : Client.t;
    key : string;
    sc_rollup_address : string;
    sc_rollup_node : Sc_rollup_node.t;
    coordinator_node : Dac_node.t;
    committee_members : Account.aggregate_key list;
    committee_members_nodes : Dac_node.t list;
    observer_nodes : Dac_node.t list;
    rollup_nodes : Sc_rollup_node.t list;
  }
end

(** [with_legacy_dac_node] creates a Legacy Dac node. DAC Committee of size
    [committee_size] will be generated and pre-configured in the legacy node.
*)
val with_legacy_dac_node :
  ?name:string ->
  ?sc_rollup_node:Sc_rollup_node.t ->
  ?pvm_name:string ->
  ?wait_ready:bool ->
  ?committee_member_address:string ->
  threshold:int ->
  committee_size:int ->
  Node.t ->
  Client.t ->
  (Dac_node.t -> Account.aggregate_key list -> 'a Lwt.t) ->
  'a Lwt.t

(** Initializes a a Coordinator Dac node. DAC Committee of size [committee_size]
    will be generated and pre-configured in the legacy node. An additional
    [custom_committee_members] can be provided to configure fixed members of the
    committee (useful for testing).
*)
val with_coordinator_node :
  ?name:string ->
  ?sc_rollup_node:Sc_rollup_node.t ->
  ?pvm_name:string ->
  ?wait_ready:bool ->
  ?custom_committee_members:Account.aggregate_key list ->
  committee_size:int ->
  Node.t ->
  Client.t ->
  (Dac_node.t -> Account.aggregate_key list -> 'a Lwt.t) ->
  'a Lwt.t

(** Initializes a a Committee Member Dac node with key [committee_member].
*)
val with_committee_member :
  ?name:string ->
  ?sc_rollup_node:Sc_rollup_node.t ->
  ?pvm_name:string ->
  ?wait_ready:bool ->
  committee_member:Account.key ->
  Node.t ->
  Dac_node.t ->
  Client.t ->
  (Dac_node.t -> Account.key -> 'a Lwt.t) ->
  'a Lwt.t

(** Initializes a Observer Dac node with key [committee_member]. *)
val with_observer :
  ?name:string ->
  ?sc_rollup_node:Sc_rollup_node.t ->
  ?pvm_name:string ->
  ?wait_ready:bool ->
  committee_member_rpcs:(string * int) list ->
  Node.t ->
  Dac_node.t ->
  Client.t ->
  (Dac_node.t -> 'b Lwt.t) ->
  'b Lwt.t

(** Initializes a new [Sc_rollup] with empty boot sector, attached to
    an [Sc_rollup_node].
*)
val with_fresh_rollup :
  ?pvm_name:string ->
  protocol:Protocol.t ->
  Node.t ->
  Client.t ->
  string ->
  (string -> Sc_rollup_node.t -> 'a Lwt.t) ->
  'a Lwt.t

(** Initalizes a scenario with full DAC infrastruture. See [Scenarios.full] for
    components.
*)
val scenario_with_full_dac_infrastructure :
  ?tags:string list ->
  ?pvm_name:string ->
  ?custom_committee_members:Account.aggregate_key list ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?node_arguments:Node.argument list ->
  __FILE__:string ->
  committee_size:int ->
  observers:int ->
  string ->
  (Scenarios.full -> unit Lwt.t) ->
  Protocol.t list ->
  unit

(** Initalizes a scenario with L1 node only. *)
val scenario_with_layer1_node :
  ?tags:string list ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?node_arguments:Node.argument list ->
  __FILE__:string ->
  string ->
  (Protocol.t -> Node.t -> Client.t -> string -> unit Lwt.t) ->
  Protocol.t list ->
  unit

(** Initalizes a scenario with L1 and legacy nodes. *)
val scenario_with_layer1_and_legacy_dac_nodes :
  ?tags:string list ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  __FILE__:string ->
  threshold:int ->
  committee_size:int ->
  string ->
  (Protocol.t ->
  Node.t ->
  Client.t ->
  Dac_node.t ->
  int ->
  Account.aggregate_key list ->
  unit Lwt.t) ->
  Protocol.t list ->
  unit

(** Initalizes a scenario with L1, legacy and rollup nodes.  *)
val scenario_with_layer1_legacy_and_rollup_nodes :
  ?tags:string list ->
  ?pvm_name:string ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?committee_member_address:string ->
  __FILE__:string ->
  threshold:int ->
  committee_size:int ->
  string ->
  (Protocol.t ->
  Dac_node.t ->
  Sc_rollup_node.t ->
  string ->
  Node.t ->
  Client.t ->
  string ->
  int ->
  Account.aggregate_key list ->
  unit Lwt.t) ->
  Protocol.t list ->
  unit
