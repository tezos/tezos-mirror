(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** This module implements an event sink (see {!Internal_event}) to emit events
    as OpenTelemetry logs. *)

(** [activate ?level ?sections ()] activates the event sink for OpenTelemetry
    logging. [level] indicate the base level for which otel logs will be emitted
    and [sections] indicate additional level filters based on sections
    prefixes. *)
val activate :
  ?level:Internal_event.level ->
  ?sections:(Internal_event.Section.t * Internal_event.level) list ->
  unit ->
  unit tzresult Lwt.t
