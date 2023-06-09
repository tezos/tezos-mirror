(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Configure the event-logging framework. *)

(** The JSON-file-friendly definition of the configuration of the
    internal-events framework. It allows one to activate registered
    event sinks.  *)

(** FIXME: https://gitlab.com/tezos/tezos/-/issues/4850
    Config override default values, but the env TEZOS_EVENTS_CONFIG
    does not. Is that what we want ? *)

open Error_monad

type t

(** The empty configuration. It doesn't activate any sink. *)
val empty : t

(** [make_config_uri kind] builds a configuration URI using the optional
    parameters with the given [kind]. The arguments are the options documented
    in [Tezos_stdlib_unix.File_descriptor_sink].
*)
val make_config_uri :
  ?level:Internal_event.Level.t ->
  ?daily_logs:int ->
  ?create_dirs:bool ->
  ?format:string ->
  ?chmod:int ->
  ?with_pid:bool ->
  ?fresh:bool ->
  ?section_prefixes:(string * Internal_event.level) list ->
  [`Stdout | `Stderr | `Path of string | `Null | `Syslog of string] ->
  Uri.t

(** The configuration uri for an stdout sink. *)
val short_stdout_uri : Uri.t

(** The configuration with only an stdout sink. *)
val stdout : t

(** Check if the configuration is empty. *)
val is_empty : t -> bool

(** Allows to make custom list of uris. *)
val make_custom : Uri.t list -> t

(** The serialization format. *)
val encoding : t Data_encoding.t

(** Run {!Tezos_base.Internal_event.All_sinks.activate} for every
      URI in the configuration. *)
val apply : t -> unit tzresult Lwt.t

(** Close all the sinks except ["lwt-log://"] and call {!apply}. *)
val reapply : t -> unit tzresult Lwt.t
