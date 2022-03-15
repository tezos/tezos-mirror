(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** This module introduces the semantics of Proof-generating Virtual Machines.

   A PVM defines an operational semantics for some computational
   model. The specificity of PVMs, in comparison with standard virtual
   machines, is their ability to generate and to validate a *compact*
   proof that a given atomic execution step turned a given state into
   another one.

   In the smart-contract rollups, PVMs are used for two purposes:

    - They allow for the externalization of rollup execution by
   completely specifying the operational semantics of a given
   rollup. This standardization of the semantics gives a unique and
   executable source of truth about the interpretation of
   smart-contract rollup inboxes, seen as a transformation of a rollup
   state.

    - They allow for the validation or refutation of a claim that the
   processing of some messages led to a given new rollup state (given
   an actual source of truth about the nature of these messages).

*)
open Alpha_context

open Sc_rollup

module type S = sig
  (**

       The state of the PVM denotes a state of the rollup.

  *)
  type state

  (** During interactive rejection games, a player may need to
      provide a proof that a given execution step is valid. *)
  type proof

  val proof_encoding : proof Data_encoding.t

  (** A state is initialized in a given context. *)
  type context

  (** A commitment hash characterized the contents of the state. *)
  type hash = State_hash.t

  (** [proof_start_state proof] returns the initial state hash of the
     [proof] execution step. *)
  val proof_start_state : proof -> hash

  (** [proof_stop_state proof] returns the final state hash of the
     [proof] execution step. *)
  val proof_stop_state : proof -> hash

  (** [state_hash state] returns a compressed representation of [state]. *)
  val state_hash : state -> hash Lwt.t

  (** [initial_state context] is the state of the PVM before booting. It must
      be such that [state_hash state = Commitment_hash.zero]. Any [context]
      should be enough to create an initial state. *)
  val initial_state : context -> string -> state Lwt.t

  (** [is_input_state state] returns [Some (level, counter)] if [state] is
      waiting for the input message that comes next to the message numbered
      [counter] in the inbox of a given [level]. *)
  val is_input_state : state -> (Raw_level.t * Z.t) option Lwt.t

  type input = {
    inbox_level : Raw_level.t;
    message_counter : Z.t;
    payload : string;
  }

  (** [set_input level n msg state] sets [msg] in [state] as
     the next message to be processed. This input message is assumed
     to be the number [n] in the inbox messages at the given
     [level]. The input message must be the message next to the
     previous message processed by the rollup. *)
  val set_input : input -> state -> state Lwt.t

  (** [eval s0] returns a state [s1] resulting from the
       execution of an atomic step of the rollup at state [s0]. *)
  val eval : state -> state Lwt.t

  (** [verify_proof input proof] returns [true] iff the [proof] is valid.
      If the state is an input state, [input] is the hash of the input
      message externally provided to the evaluation function. *)
  val verify_proof : input:input option -> proof -> bool Lwt.t
end
