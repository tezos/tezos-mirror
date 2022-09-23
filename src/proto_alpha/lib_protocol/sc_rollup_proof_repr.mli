(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** A refutation game proof is required as part of the final move in a
    game.

    This proof is basically a combination of a PVM proof (provided by
    each implementation of the PVM signature) and an input proof. To
    check the proof we must check each part separately and then also
    check that they match on the two points where they touch:

      - the [input_requested] of the PVM proof should match the starting
      point of the input proof ;

      - the [input_given] of the PVM proof should match the output
      message of the input proof.

    It is also often the case that the PVM proof has [No_input_required]
    for its [input_requested] and [None] for its [input_given]. If this
    is the case, we don't need the input proof at all and the [input_proof]
    parameter in our proof should be [None]. *)

open Sc_rollup_repr

(** The proof that a reveal is valid. *)
type reveal_proof =
  | Raw_data_proof of string
      (** The existence of reveal for a given hash when the
          [input_requested] is the [Needs_for_reveal]. *)

(** A PVM proof [pvm_step] is combined with an [input_proof] to provide
    the proof necessary to validate a single step in the refutation
    game.

    If the step doesn't involve any input, [proof_input_requested
    pvm_step] and [proof_input_given pvm_step] will be
    [No_input_required] and [None] respectively, and in this case
    [inbox] should also be [None].

    In the case that input is involved, [input_proof] is either:

    - a proof of the next inbox message available from the inbox
      after a given location; this must match up with [pvm_step]
      to give a valid refutation proof ; or

    - a proof of a reveal satisfiability.
*)

type input_proof =
  | Inbox_proof of {
      level : Raw_level_repr.t;
      message_counter : Z.t;
      proof : Sc_rollup_inbox_repr.serialized_proof;
    }
  | Reveal_proof of reveal_proof

type t = {pvm_step : Sc_rollups.wrapped_proof; input_proof : input_proof option}

type error += Sc_rollup_proof_check of string

type error += Sc_rollup_invalid_serialized_inbox_proof

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** The state hash of the machine before the step. This must be checked
    against the value in the refutation game as well as checking the
    proof is valid. *)
val start : t -> State_hash.t

(** The state hash of the machine after the step. This must be checked
    against the value in the refutation game as well as checking the
    proof is valid. *)
val stop : t -> State_hash.t

(** Check the validity of a proof.

    This function requires a few bits of data (available from the
    refutation game record in the storage):

      - a snapshot of the inbox, that may be used by the [input] proof ;

      - the inbox level of the commitment, used to determine if an
        output from the [input] proof is too recent to be allowed into
        the PVM proof ;

      - the [pvm_name], used to check that the proof given has the right
        PVM kind.

    It also returns the optional input executed during the proof and the
    input_request for the state at the beginning of the proof.
*)
val valid :
  Sc_rollup_inbox_repr.history_proof ->
  Raw_level_repr.t ->
  pvm_name:string ->
  t ->
  (Sc_rollup_PVM_sig.input option * Sc_rollup_PVM_sig.input_request) tzresult
  Lwt.t

module type PVM_with_context_and_state = sig
  include Sc_rollups.PVM.S

  val context : context

  val state : state

  val proof_encoding : proof Data_encoding.t

  val reveal : Sc_rollup_PVM_sig.Input_hash.t -> string option

  module Inbox_with_history : sig
    include
      Sc_rollup_inbox_repr.Merkelized_operations
        with type inbox_context = context

    val inbox : Sc_rollup_inbox_repr.history_proof

    val history : Sc_rollup_inbox_repr.History.t
  end
end

(** [produce pvm_and_state inbox_context inbox_history commit_level]
    will construct a full refutation game proof out of the [state] given
    in [pvm_and_state].  It uses the [inbox] if necessary to provide
    input in the proof. If the input is above or at [commit_level] it
    will block it, and produce a proof that the PVM is blocked. If
    the input requested is a reveal the proof production will also
    fail.

    This will fail if any of the [context], [inbox_context] or
    [inbox_history] given don't have enough data to make the proof. For
    example, the 'protocol implementation' version of each PVM won't be
    able to run this function. Similarly, the version of the inbox
    stored in the L1 won't be enough because it forgets old levels.

    This uses the [name] in the [pvm_and_state] module to produce an
    encodable [wrapped_proof] if possible. See the [wrap_proof] function
    in [Sc_rollups]. *)
val produce :
  (module PVM_with_context_and_state) -> Raw_level_repr.t -> t tzresult Lwt.t
