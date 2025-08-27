(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type gc_telemetry = {
  enable : bool;
  filter : (Runtime_events.runtime_phase -> bool) option;
  min_duration_ms : float option;
}

(** Type for opentelemetry configuration. *)
type t = {
  enable : bool;
      (** When this is false, opentelemetry should not be enabled. *)
  instance_id : string option;
      (** User provided instance identifier for opentelemetry traces. *)
  environment : string option;
      (** Deployment environment used for Datadog tagging.  *)
  config : Opentelemetry_client_cohttp_lwt.Config.t;
      (** The actual configuration to use for opentelemetry when [enable = true]. *)
  gc_telemetry : gc_telemetry;
      (** Configuration to enable telemetry traces for GC events. *)
}

(** Default config, disabled. *)
val default : t

(** Encoding, to be used in configuration files. *)
val encoding : t Data_encoding.t

val extended_encoding : 'a Data_encoding.t -> 'a -> (t * 'a) Data_encoding.t

(** Set enable field. *)
val enable : t -> bool -> t
