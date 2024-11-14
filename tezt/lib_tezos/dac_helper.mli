(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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
  (** Components for full DAC infrastructure. *)
  type full = {
    protocol : Protocol.t;
    node : Node.t;
    client : Client.t;
    key : string;
    sc_rollup_address : string;
    sc_rollup_node : Sc_rollup_node.t;
    coordinator_node : Dac_node.t;
    committee_members : Account.key list;
    committee_members_nodes : Dac_node.t list;
    observer_nodes : Dac_node.t list;
    rollup_nodes : Sc_rollup_node.t list;
  }
end

(** Initializes a a Coordinator Dac node. DAC Committee of size [committee_size]
    will be generated and pre-configured in the Coordfinator node. An additional
    [custom_committee_members] can be provided to configure fixed members of the
    committee (useful for testing). *)
val with_coordinator_node :
  ?name:string ->
  ?sc_rollup_node:Sc_rollup_node.t ->
  ?pvm_name:string ->
  ?wait_ready:bool ->
  ?allow_v1_api:bool ->
  committee_members:Account.key list ->
  Node.t ->
  Client.t ->
  (Dac_node.t -> Account.key list -> 'a Lwt.t) ->
  'a Lwt.t

(** Initializes a a Committee Member Dac node with key [committee_member].
*)
val with_committee_member :
  ?name:string ->
  ?sc_rollup_node:Sc_rollup_node.t ->
  ?pvm_name:string ->
  ?wait_ready:bool ->
  ?allow_v1_api:bool ->
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
  ?allow_v1_api:bool ->
  committee_member_rpcs:(string * int) list ->
  Node.t ->
  Dac_node.t ->
  Client.t ->
  (Dac_node.t -> 'b Lwt.t) ->
  'b Lwt.t

(** Initializes a new [Sc_rollup] with empty boot sector, attached to
    an [Sc_rollup_node]. *)
val with_fresh_rollup :
  ?pvm_name:string ->
  ?hooks:Process_hooks.t ->
  Node.t ->
  Client.t ->
  string ->
  (string -> Sc_rollup_node.t -> 'a Lwt.t) ->
  'a Lwt.t

(** Initalizes a scenario with full DAC infrastruture. See [Scenarios.full] for
    components. *)
val scenario_with_full_dac_infrastructure :
  ?supports:Protocol.supported_protocols ->
  ?tags:string list ->
  ?uses:(Protocol.t -> Uses.t list) ->
  ?pvm_name:string ->
  ?custom_committee_members:Account.key list ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?node_arguments:Node.argument list ->
  ?allow_v1_api:bool ->
  ?allow_regression:bool ->
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
  ?uses:(Protocol.t -> Uses.t list) ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?node_arguments:Node.argument list ->
  __FILE__:string ->
  string ->
  (Protocol.t -> Node.t -> Client.t -> string -> unit Lwt.t) ->
  Protocol.t list ->
  unit

(** This module is syntactix sugar to call DAC RPC endpoints. *)
module Call_endpoint : sig
  module V0 : sig
    (** Call GET v0/preimage/page_hash for the provided [page_hash]. *)
    val get_preimage : Dac_node.t -> string -> string Lwt.t

    (** Call PUT v0/dac_member_signature for the provided [hex_root_hash],
        [dac_member_pkh] and [signature]. *)
    val put_dac_member_signature :
      Dac_node.t ->
      hex_root_hash:Hex.t ->
      dac_member_pkh:string ->
      signature:Tezos_crypto.Aggregate_signature.signature ->
      unit Lwt.t

    (** Call GET /v0/missing_page for the provided [hex_root_hash]. *)

    val get_missing_page : Dac_node.t -> hex_root_hash:Hex.t -> string Lwt.t

    (** Call GET /v0/certificate for the provided [hex_root_hash]. *)
    val get_certificate :
      Dac_node.t -> hex_root_hash:Hex.t -> (int * string * string * int) Lwt.t

    module Coordinator : sig
      (** Call POST /v0/preimage for the provided payload. *)
      val post_preimage : Dac_node.t -> payload:string -> string Lwt.t
    end
  end

  module V1 : sig
    (** Call GET /v1/pages for the provided [page_hash]. *)
    val get_pages : Dac_node.t -> string -> string Lwt.t
  end

  (** Call GET /health/live. *)
  val get_health_live : Dac_node.t -> bool Lwt.t

  (** Call GET /health/ready. *)
  val get_health_ready : Dac_node.t -> bool Lwt.t
end
