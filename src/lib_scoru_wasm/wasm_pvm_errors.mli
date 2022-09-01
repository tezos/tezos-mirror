(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Raw exception as printed by `Printexc.to_string`. *)
type raw_exception := string

(** Embedded message in the exception. *)
type explanation := string

(** Wrapped exceptions from the interpreter.  *)
type interpreter_error = {
  raw_exception : raw_exception;
  explanation : string option;
}

type t =
  | Decode_error of interpreter_error
      (** Wraps exceptions raised during parsing. *)
  | Init_error of interpreter_error
      (** Wraps exceptions raised during initialization. *)
  | Eval_error of interpreter_error
      (** Wraps exceptions raised during evaluation. *)
  | Invalid_state of explanation
      (** Invalid state of the PVM (waiting for input during the parsing for example). *)
  | Unknown_error of raw_exception
      (** Wraps unexpected exceptions raised by the interpreter. *)

(** [is_interpreter_error exn] returns true if the exception comes
    from the interpreter. *)

val is_interpreter_error : exn -> bool

val refine_error : exn -> interpreter_error

val encoding : t Data_encoding.t
