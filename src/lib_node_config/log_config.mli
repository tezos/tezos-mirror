(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** This module handles default configuration for logs specific to the node *)

(** Default value for the lwt_log sink *)
val lwt_log_sink_default_cfg : Lwt_log_sink_unix.cfg

(** Creates internal event configuration using default node values depending on
parameters *)
val make_internal_events_with_defaults :
  ?internal_events:Internal_event_config.t * string ->
  ?verbosity:Internal_event.level ->
  ?log_cfg:Lwt_log_sink_unix.cfg ->
  unit ->
  Lwt_log_sink_unix.cfg * Internal_event_config.t

(** Inits internal event configuration using default node values depending on
parameters *)
val init_internal_events_with_defaults :
  ?internal_events:Internal_event_config.t * string ->
  ?verbosity:Internal_event.level ->
  ?log_cfg:Lwt_log_sink_unix.cfg ->
  unit ->
  unit Lwt.t
