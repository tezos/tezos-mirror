(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Prometheus' configuration. *)
type prometheus = {metrics_addr : string; metrics_port : int}

(** Governance contracts addresses. *)
type contracts = {
  sequencer_governance : string;
  kernel_governance : string;
  security_kernel_governance : string;
}

(** Mainnet contracts configuration. *)
val mainnet_contracts : contracts

(** Ghostnet contracts configuration. *)
val ghostnet_contracts : contracts

(** Configuration for the governance observer. *)
type configuration = {
  prometheus : prometheus;
  endpoint : Uri.t;
  contracts : contracts;
}

type network_argument_not_found_error = Both_args | No_args

(** [network_argument_not_found ~error] will output an error message based on
    [error] related to the user providing 0 or too much network arguments.
    This function will make the program exit. *)
val network_argument_not_found : error:network_argument_not_found_error -> 'a

module Args : sig
  (** Metrics address of the prometheus server. *)
  val metrics_addr : (string, unit) Tezos_clic.arg

  (** Metrics port of the prometheus server. *)
  val metrics_port : (int, unit) Tezos_clic.arg

  (** RPC endpoint (L1 octez node). *)
  val endpoint : (string, unit) Tezos_clic.arg

  (** Enable configuration for mainnet's governance contracts. *)
  val etherlink_mainnet : (bool, unit) Tezos_clic.arg

  (** Enable configuration for ghostnet's governance contracts. *)
  val etherlink_ghostnet : (bool, unit) Tezos_clic.arg
end
