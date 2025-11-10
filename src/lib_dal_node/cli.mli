(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Types = Tezos_dal_node_services.Types

module Term : sig
  type env = {docs : string; doc : string; name : string}

  type 'a arg = {
    default : 'a option;
    short : char option;
    long : string;
    extra_long : string list;
    parse : string -> ('a, string) result;
    doc : string;
    placeholder : string;
    pp : Format.formatter -> 'a -> unit;
    env : env option;
  }

  type switch = {long : string; extra_long : string list; doc : string}

  val data_dir_arg : string arg

  val config_file_arg : string arg

  val rpc_addr_arg : P2p_point.Id.t arg

  val expected_pow_arg : float arg

  val net_addr_arg : P2p_point.Id.t arg

  val public_addr_arg : P2p_point.Id.t arg

  val endpoint_arg : Uri.t arg

  val slots_backup_uris_arg : Uri.t list arg

  val trust_slots_backup_uris_switch : switch

  val ignore_l1_config_peers_switch : switch

  val attester_profile_arg : Signature.public_key_hash list arg

  val operator_profile_arg : int list arg

  val observer_profile_arg : int trace arg

  val bootstrap_profile_switch : switch

  val peers_arg : string list arg

  val metrics_addr_arg : P2p_point.Id.t arg

  val history_mode_arg : Configuration_file.history_mode arg

  val service_name_arg : string arg

  val service_namespace_arg : string arg

  val fetch_trusted_setup_arg : bool arg

  val disable_shard_validation_switch : switch

  val disable_amplification_switch : switch

  val verbose_switch : switch

  val ignore_topics_arg : Signature.public_key_hash list arg

  val batching_configuration_arg : Configuration_file.batching_configuration arg
end

(** {2 Command-line options} *)

(** This module declares the main commands of the DAL node. For each
    command, a set of options is recognized. The function [commands]
    can be used to register a function when the user invokes the
    command. *)

type experimental_features = unit

type options = {
  data_dir : string option;
      (** Directory containing files related to the DAL node. *)
  config_file : string option;  (** Configuration file of the DAL node. *)
  rpc_addr : P2p_point.Id.t option;
      (** The endpoint on which the DAL node can be contacted for RPCs. *)
  expected_pow : float option;
      (** The expected proof of work for the P2P identity. *)
  listen_addr : P2p_point.Id.t option;
      (** The TCP address and port bound by the DAL node. *)
  public_addr : P2p_point.Id.t option;
      (** The endpoint on which the DAL node can be contacted by other DAL nodes. *)
  endpoint : Uri.t option;  (** The endpoint on which to contact the L1 node. *)
  slots_backup_uris : Uri.t list;
      (** (Optional) URIs to use as backup sources for slot data retrieval, in
          case the slot is missing locally and reconstruction from shards is not
          possible. Supported URI schemes include [http], [https], and
          [file]. *)
  trust_slots_backup_uris : bool;
      (** Whether to trust the data downlaoded from the provided slots backup URIs. *)
  profile : Profile_manager.unresolved_profile option;
      (** Profiles of the DAL node used for tracking shards. *)
  metrics_addr : P2p_point.Id.t option;  (** Metrics server endpoint. *)
  peers : string list;  (** DAL nodes to connect to. *)
  history_mode : Configuration_file.history_mode option;
  service_name : string option;
      (** Name of the service provided by this node. *)
  service_namespace : string option;  (** Namespace for the service. *)
  experimental_features : experimental_features;  (** Experimental features. *)
  fetch_trusted_setup : bool option;
      (** Should the trusted setup be installed if required and invalid?
      In case of [None] at init it is considered as yes. *)
  disable_shard_validation : bool;
      (** Should the crypto shard verification against commitment hashes be bypassed. *)
  verbose : bool;
      (** Emit events related to connections. Default value is false. *)
  ignore_l1_config_peers : bool;
      (** Ignore the boot(strap) peers provided by L1. *)
  disable_amplification : bool;
      (** Disable amplification. Default value is false. *)
  ignore_topics : Signature.public_key_hash list;
      (** Do not distribute shards of these pkhs. *)
  batching_configuration : Configuration_file.batching_configuration option;
      (** The configuration used for batching verification of received shards
          via GossipSub to save cryptographic computation. *)
}

(** Subcommands that can be used by the DAL node. In the future this type
    could be generalized if a command recgonizes a different set of
    options. *)
type t = Run | Config_init | Config_update | Debug_print_store_schemas

val cli_options_to_options :
  ?data_dir:string ->
  ?config_file:string ->
  ?rpc_addr:P2p_point.Id.t ->
  ?expected_pow:float ->
  ?listen_addr:P2p_point.Id.t ->
  ?public_addr:P2p_point.Id.t ->
  ?endpoint:Uri.t ->
  ?slots_backup_uris:Uri.t list ->
  ?trust_slots_backup_uris:bool ->
  ?metrics_addr:P2p_point.Id.t ->
  ?attesters:Signature.public_key_hash list ->
  ?operators:int list ->
  ?observers:int list ->
  ?bootstrap:bool ->
  ?peers:string list ->
  ?history_mode:Configuration_file.history_mode ->
  ?service_name:string ->
  ?service_namespace:string ->
  ?fetch_trusted_setup:bool ->
  ?disable_shard_validation:bool ->
  ?verbose:bool ->
  ?ignore_l1_config_peers:bool ->
  ?disable_amplification:bool ->
  ?ignore_topics:Signature.public_key_hash list ->
  ?batching_configuration:Configuration_file.batching_configuration ->
  unit ->
  (options, bool * string) result

val run : t -> options -> unit tzresult Lwt.t

(** Subcommands of the DAL node *)
val commands : unit Cmdliner.Cmd.t
