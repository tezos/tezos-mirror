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

  type 'a arg_list = {
    default : 'a trace;
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

  val rpc_addr_arg : P2p_point.Id.t arg

  val expected_pow_arg : float arg

  val net_addr_arg : P2p_point.Id.t arg

  val public_addr_arg : P2p_point.Id.t arg

  val endpoint_arg : Uri.t arg

  val http_backup_uris_arg : Uri.t arg_list

  val trust_http_backup_uris_switch : switch

  val ignore_l1_config_peers_switch : switch

  val attester_profile_arg : Signature.public_key_hash arg_list

  val operator_profile_arg : int arg_list

  val observer_profile_arg : int trace arg

  val bootstrap_profile_switch : switch

  val peers_arg : string arg_list

  val metrics_addr_arg : P2p_point.Id.t arg

  val history_mode_arg : Configuration_file.history_mode arg

  val service_name_arg : string arg

  val service_namespace_arg : string arg

  val fetch_trusted_setup_arg : bool arg

  val verbose_switch : switch
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
  rpc_addr : P2p_point.Id.t option;
      (** The endpoint on which the DAL node can be contacted for RPCs. *)
  expected_pow : float option;
      (** The expected proof of work for the P2P identity. *)
  listen_addr : P2p_point.Id.t option;
      (** The TCP address and port bound by the DAL node. *)
  public_addr : P2p_point.Id.t option;
      (** The endpoint on which the DAL node can be contacted by other DAL nodes. *)
  endpoint : Uri.t option;  (** The endpoint on which to contact the L1 node. *)
  http_backup_uris : Uri.t list;
      (** (Optional) URIs to use as HTTP backup sources for slot data retrieval,
          in case the slot is missing locally and reconstruction from shards is
          not possible. *)
  trust_http_backup_uris : bool;
      (** Whether to trust the data downlaoded from the provided HTTP backup URIs. *)
  profile : Profile_manager.unresolved_profile option;
      (** Profiles of the DAL node used for tracking shards. *)
  metrics_addr : P2p_point.Id.t option;  (** Metrics server endpoint. *)
  peers : string list;  (** DAL nodes to connect to. *)
  history_mode : Configuration_file.history_mode option;
  service_name : string option;
      (** Name of the service provided by this node. *)
  service_namespace : string option;  (** Namespace for the service. *)
  experimental_features : experimental_features;  (** Experimental features.  *)
  fetch_trusted_setup : bool option;
      (** Should the trusted setup be installed if required and invalid?
      In case of [None] at init it is considered as yes.*)
  verbose : bool;
      (** Emit events related to connections. Default value is false. *)
  ignore_l1_config_peers : bool;
      (** Ignore the boot(strap) peers provided by L1. *)
}

(** Subcommands that can be used by the DAL node. In the future this type
    could be generalized if a command recgonizes a different set of
    options. *)
type t = Run | Config_init | Config_update | Debug_print_store_schemas

val cli_options_to_options :
  string option ->
  P2p_point.Id.t option ->
  float option ->
  P2p_point.Id.t option ->
  P2p_point.Id.t option ->
  Uri.t option ->
  Uri.t list ->
  bool ->
  P2p_point.Id.t option ->
  Signature.public_key_hash trace ->
  int trace ->
  int trace option ->
  bool ->
  string trace ->
  Configuration_file.history_mode option ->
  string option ->
  string option ->
  bool option ->
  bool ->
  bool ->
  (options, bool * string) result

val run : ?disable_logging:bool -> t -> options -> unit tzresult Lwt.t

(** Subcommands of the DAL node *)
val commands : unit Cmdliner.Cmd.t
