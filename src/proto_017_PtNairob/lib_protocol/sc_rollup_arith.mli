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

   It processes postfix arithmetic expressions written as sequence of
   (space separated) [int] and [+] using the following rules:

   - a number [x] is interpreted as pushing [x] on the stack ;

   - a variable [a] is interpreted as storing the topmost element of the
     stack in the storage under the name "a" ;

   - a variable [out] is interpreted as adding a message to the outbox
     containing a single transaction batch with the topmost element of the
     stack as payload, the zero contract as destination, and a default
     entrypoint ;

   - a symbol [+] pops two integers [x] and [y] and pushes [x + y] on
     the stack ;

   - an input [hash:<HASH>] is interpreted as a directive to request the DAC
     data whose hash is <HASH> ;

   - an input [dal:<e>:<num_p>:<s1>:<s2>:...:<sn>] is interpreted as a directive
   to provide the DAL parameters to the PVM, where:
     - <e> is the attestation lag;
     - <num_p> is the number of pages;
     - each <si> is a slot to which the PVM subscribes to for [current level -
     attestation_lag - 1].

    DAL parameters can be set at most once. At each Start_of_level of some level
    [L] inbox message and if DAL is enabled for the rollup (via the directive
    above), the PVM will request the pages of the slots it is subscribed to. The
    (attested) slots that are actually fetched at level [L] are those published
    at level [L - e - 1]. Note that providing some DAC data via a DAL page will
    prevent from fetching the subsequent DAL pages.

   If a message is not syntactically correct or does not evaluate
   correctly, the machine stops its evaluation and waits for the next
   message.

   The machine has a boot sector which is a mere string used a prefix
   for each message.

   The module implements the {!Sc_rollup_PVM_sig.S}Î interface to be
   used in the smart contract rollup infrastructure.

   The machine exposes extra operations to be used in the rollup node.

*)
module type S = sig
  include Sc_rollup_PVM_sig.S

  (** [parse_boot_sector s] builds a boot sector from its human
      writable description. *)
  val parse_boot_sector : string -> string option

  (** [pp_boot_sector fmt s] prints a human readable representation of
     a boot sector. *)
  val pp_boot_sector : Format.formatter -> string -> unit

  (** [pp state] returns a pretty-printer valid for [state]. *)
  val pp : state -> (Format.formatter -> unit -> unit) Lwt.t

  (** [get_tick state] returns the current tick of [state]. *)
  val get_tick : state -> Sc_rollup_tick_repr.t Lwt.t

  (** The machine has five possible statuses: *)
  type status =
    | Halted
    | Waiting_for_input_message
    | Waiting_for_reveal
    | Waiting_for_metadata
    | Parsing
    | Evaluating

  (** [get_status state] returns the machine status in [state]. *)
  val get_status : state -> status Lwt.t

  (** [get_outbox outbox_level state] returns the outbox in [state]
      for a given [outbox_level]. *)
  val get_outbox :
    Raw_level_repr.t -> state -> Sc_rollup_PVM_sig.output list Lwt.t

  (** The machine has only three instructions. *)
  type instruction =
    | IPush : int -> instruction
    | IAdd : instruction
    | IStore : string -> instruction

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

  (** [get_var state x] returns the current value of variable [x].
      Returns [None] if [x] does not exist. *)
  val get_var : state -> string -> int option Lwt.t

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

module Protocol_implementation :
  S
    with type context = Context.t
     and type state = Context.tree
     and type proof = Context.Proof.tree Context.Proof.t

(** This is the state hash of reference that both the prover of the
    node and the verifier of the protocol {!Protocol_implementation}
    have to agree on (if they do, it means they are using the same
    tree structure). *)
val reference_initial_state_hash : Sc_rollup_repr.State_hash.t

module type P = sig
  module Tree : Context.TREE with type key = string list and type value = bytes

  type tree = Tree.tree

  val hash_tree : tree -> Sc_rollup_repr.State_hash.t

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_before : proof -> Sc_rollup_repr.State_hash.t

  val proof_after : proof -> Sc_rollup_repr.State_hash.t

  val verify_proof :
    proof -> (tree -> (tree * 'a) Lwt.t) -> (tree * 'a) option Lwt.t

  val produce_proof :
    Tree.t -> tree -> (tree -> (tree * 'a) Lwt.t) -> (proof * 'a) option Lwt.t
end

module Make (Context : P) :
  S
    with type context = Context.Tree.t
     and type state = Context.tree
     and type proof = Context.proof
