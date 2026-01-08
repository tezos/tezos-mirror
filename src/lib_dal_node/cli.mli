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

  (** Directory containing files related to the DAL node. *)
  val data_dir_arg : string arg

  (** Configuration file of the DAL node. *)
  val config_file_arg : string arg

  (** The endpoint on which the DAL node can be contacted for RPCs. *)
  val rpc_addr_arg : P2p_point.Id.t arg

  (** The expected proof of work for the P2P identity. *)
  val expected_pow_arg : float arg

  (** The TCP address and port bound by the DAL node. *)
  val net_addr_arg : P2p_point.Id.t arg

  (** The endpoint on which the DAL node can be contacted by other DAL nodes. *)
  val public_addr_arg : P2p_point.Id.t arg

  (** The endpoint on which to contact the L1 node. *)
  val endpoint_arg : Uri.t arg

  (** (Optional) URIs to use as backup sources for slot data retrieval, in
      case the slot is missing locally and reconstruction from shards is not
      possible. Supported URI schemes include [http], [https], and
      [file]. *)
  val slots_backup_uris_arg : Uri.t list arg

  (** Whether to trust the data downloaded from the provided slots backup URIs. *)
  val trust_slots_backup_uris_switch : switch

  (** Ignore the boot(strap) peers provided by L1. *)
  val ignore_l1_config_peers_switch : switch

  val attester_profile_arg : Signature.public_key_hash list arg

  val operator_profile_arg : int list arg

  val observer_profile_arg : int trace arg

  val bootstrap_profile_switch : switch

  (** DAL nodes to connect to. *)
  val peers_arg : string list arg

  (** Metrics server endpoint. *)
  val metrics_addr_arg : P2p_point.Id.t arg

  val history_mode_arg : Configuration_file.history_mode arg

  (** Name of the service provided by this node. *)
  val service_name_arg : string arg

  (** Namespace for the service. *)
  val service_namespace_arg : string arg

  (** Should the trusted setup be installed if required and invalid? *)
  val fetch_trusted_setup_arg : bool arg

  (** Should the crypto shard verification against commitment hashes be bypassed. *)
  val disable_shard_validation_switch : switch

  (** Disable amplification. Default value is false. *)
  val disable_amplification_switch : switch

  (** Emit events related to connections. Default value is false. *)
  val verbose_switch : switch

  (** Do not distribute shards of these pkhs. *)
  val ignore_topics_arg : Signature.public_key_hash list arg

  (** The configuration used for batching verification of received shards
      via GossipSub to save cryptographic computation. *)
  val batching_configuration_arg : Configuration_file.batching_configuration arg

  val publish_slots_regularly_arg :
    Configuration_file.publish_slots_regularly arg
end

(** {2 Command-line subcommands} *)

module Action : sig
  (** Optional boolean values have [false] as default value. *)

  val run :
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
    ?publish_slots_regularly:Configuration_file.publish_slots_regularly ->
    unit ->
    (unit, Error_monad.tztrace) result Lwt.t

  val config_init :
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
    ?verbose:bool ->
    ?ignore_l1_config_peers:bool ->
    ?disable_amplification:bool ->
    ?batching_configuration:Configuration_file.batching_configuration ->
    unit ->
    (unit, Error_monad.tztrace) result Lwt.t

  val config_update :
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
    ?verbose:bool ->
    ?ignore_l1_config_peers:bool ->
    ?disable_amplification:bool ->
    ?batching_configuration:Configuration_file.batching_configuration ->
    unit ->
    (unit, Error_monad.tztrace) result Lwt.t

  val debug_print_store_schemas : unit -> (unit, tztrace) result Lwt.t
end

(** Subcommands of the DAL node *)
val commands : int Cmdliner.Cmd.t
