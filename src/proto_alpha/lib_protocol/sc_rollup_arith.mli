(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module provides a temporary toy rollup to be used as a demo. *)

(**

   This rollup is a stack machine equipped with addition.

   It processed postfix arithmetic expressions written as sequence of
   (space separated) [int] and [+] using the following rules:

   - a number [x] is interpreted as pushing [x] on the stack ;

   - a symbol [+] pops two integers [x] and [y] and pushes [x + y] on
   the stack.

   If a message is not syntactically correct or does not evaluate
   correctly, the machine stops its evaluation and waits for the next
   message.

   The machine has a boot sector which is a mere string used a prefix
   for each message.

   The module implements the {!Sc_rollup_PVM_sem.S} interface to be
   used in the smart contract rollup infrastructure.

   The machine exposes extra operations to be used in the rollup node.

*)
open Alpha_context

module type S = sig
  include Sc_rollup_PVM_sem.S

  (** [name] is "arith". *)
  val name : string

  (** [parse_boot_sector s] builds a boot sector from its human
      writable description. *)
  val parse_boot_sector : string -> string option

  (** [pp_boot_sector fmt s] prints a human readable representation of
     a boot sector. *)
  val pp_boot_sector : Format.formatter -> string -> unit

  (** [pp state] returns a pretty-printer valid for [state]. *)
  val pp : state -> (Format.formatter -> unit -> unit) Lwt.t

  (** [get_tick state] returns the current tick of [state]. *)
  val get_tick : state -> Sc_rollup.Tick.t Lwt.t

  (** The machine has three possible states: *)
  type status = Halted | WaitingForInputMessage | Parsing | Evaluating

  (** [get_status state] returns the machine status in [state]. *)
  val get_status : state -> status Lwt.t

  (** The machine has only two instructions. *)
  type instruction = IPush : int -> instruction | IAdd : instruction

  (** [equal_instruction i1 i2] is [true] iff [i1] equals [i2]. *)
  val equal_instruction : instruction -> instruction -> bool

  (** [pp_instruction fmt i] shows a human readable representation of [i]. *)
  val pp_instruction : Format.formatter -> instruction -> unit

  (** [get_parsing_result state] is [Some true] if the current
      message is syntactically correct, [Some false] when it
      contains a syntax error, and [None] when the machine is
      not in parsing state. *)
  val get_parsing_result : state -> bool option Lwt.t

  (** [get_code state] returns the current code obtained by parsing
      the current input message. *)
  val get_code : state -> instruction list Lwt.t

  (** [get_stack state] returns the current stack. *)
  val get_stack : state -> int list Lwt.t

  (** [get_evaluation_result state] returns [Some true] if the current
      message evaluation succeeds, [Some false] if it failed, and
      [None] if the evaluation has not been done yet. *)
  val get_evaluation_result : state -> bool option Lwt.t

  (** [get_is_stuck state] returns [Some err] if some internal error
      made the machine fail during the last evaluation step. [None]
      if no internal error occurred. When a machine is stuck, it
      reboots, waiting for the next message to process. *)
  val get_is_stuck : state -> string option Lwt.t
end

module ProtocolImplementation : S with type context = Context.t

module type P = sig
  module Tree : Context.TREE with type key = string list and type value = bytes

  type tree = Tree.tree

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_start_state : proof -> Sc_rollup.State_hash.t

  val proof_stop_state : proof -> Sc_rollup.State_hash.t

  val verify_proof :
    proof ->
    (tree -> (tree * 'a) Lwt.t) ->
    ( tree * 'a,
      [ `Proof_mismatch of string
      | `Stream_too_long of string
      | `Stream_too_short of string ] )
    result
    Lwt.t
end

module Make (Context : P) :
  S with type context = Context.Tree.t and type state = Context.tree
