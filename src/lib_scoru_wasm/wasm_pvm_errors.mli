(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** Representation of errors of the WASM PVM. *)

(** Stuck messages are purely informational, but can be arbitrary long since they
   come from error messages from the interpreter or caught exceptions. To ensure
   they fit in an L1 operation, we truncate them after
   [messages_maximum_size]. *)
val messages_maximum_size : int

(** Representation of truncated strings of maximum length
    [messages_maximum_size]. Can only be built through [truncate_message]. *)
type truncated_string = private Truncated of string [@@ocaml.unboxed]

(**  [truncate_message m] builds a truncated message out of a given message
     [m]. *)
val truncate_message : string -> truncated_string

(** Raw exception as printed by `Printexc.to_string`, possibly truncated. *)
type raw_exception := truncated_string

(** Embedded message in the exception, possibly truncated. *)
type explanation := truncated_string

(** Wrapped exceptions from the interpreter. Both labels are truncated after
    [messages_maximum_size]. *)
type interpreter_error = {
  raw_exception : raw_exception;
  explanation : explanation option;
}

type fallback_cause =
  | Decode_cause of interpreter_error
      (** Wraps exceptions raised during parsing. *)
  | Link_cause of explanation
      (** Errors or possible raw exceptions raised during linking. *)
  | Init_cause of interpreter_error
      (** Wraps exceptions raised during initialization. *)

type t =
  | Decode_error of interpreter_error
      (** Wraps exceptions raised during parsing. *)
  | Link_error of explanation
      (** Errors or possible raw exceptions raised during linking. *)
  | Init_error of interpreter_error
      (** Wraps exceptions raised during initialization. *)
  | Eval_error of interpreter_error
      (** Wraps exceptions raised during evaluation. *)
  | Invalid_state of explanation
      (** Invalid state of the PVM (waiting for input during the parsing for example). *)
  | Unknown_error of raw_exception
      (** Wraps unexpected exceptions raised by the interpreter. *)
  | Too_many_ticks
      (** The maximum number of ticks was reached before the end of current top level call *)
  | Too_many_reboots
      (** The maximum number of reboots was reached before the next inputs *)
  | No_fallback_kernel of fallback_cause
      (** No fallback kernel was available - recovering from invalid kernel module not possible. *)

(* [invalid_state msg] builds an `Invalid_state` error out of a message. *)
val invalid_state : string -> t

(* [link_error kind ~module_name ~item_name] returns the link error for a given
   [module_name] and [item_name], and the kind of error (whether an unkown
   module or item). *)
val link_error :
  [`Item | `Module] -> module_name:string -> item_name:string -> t

(** [extract_interpreter_error exn] returns the source of the exception (either
    a known interpreter error or an unknown one) and its encodable
    representation. [raw_exception] is truncated after
    [messages_maximum_size]. *)
val extract_interpreter_error :
  exn -> [`Interpreter of interpreter_error | `Unknown of raw_exception]

val encoding : t Data_encoding.t
