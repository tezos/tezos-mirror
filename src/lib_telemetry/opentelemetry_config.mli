(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Type for opentelemetry configuration. *)
type t = {
  enable : bool;
      (** When this is false, opentelemetry should not be enabled. *)
  instance_id : string option;
      (** User provided instance identifier for opentelemetry traces. *)
  config : Opentelemetry_client_cohttp_lwt.Config.t;
      (** The actual configuration to use for opentelemetry when [enable = true]. *)
}

(** Default config, disabled. *)
val default : t

(** Encoding, to be used in configuration files. *)
val encoding : t Data_encoding.t
