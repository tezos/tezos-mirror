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
    each implementation of the PVM signature) and an inbox proof. To
    check the proof we must check each part separately and then also
    check that they match on the two points where they touch:

      - the [input_requested] of the PVM proof should match the starting
      point of the inbox proof ;

      - the [input_given] of the PVM proof should match the output
      message of the inbox proof.

    It is also often the case that the PVM proof has [No_input_required]
    for its [input_requested] and [None] for its [input_given]. If this
    is the case, we don't need the inbox proof at all and the [inbox]
    parameter in our proof should be [None]. *)

open Sc_rollup_repr

(** A PVM proof [pvm_step] is combined with an [inbox] proof to provide
    the proof necessary to validate a single step in the refutation
    game.

    If the step doesn't involve any input, [proof_input_requested
    pvm_step] and [proof_input_given pvm_step] will be
    [No_input_required] and [None] respectively, and in this case
    [inbox] should also be [None].

    In the case that input is involved, [inbox] is a proof of the next
    message available from the inbox after a given location; this must
    match up with [pvm_step] to give a valid refutation proof. *)
type t = {
  pvm_step : Sc_rollups.wrapped_proof;
  inbox : Sc_rollup_inbox_repr.Proof.t option;
}

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** The state hash of the machine before the step. This must be checked
    against the value in the refutation game as well as checking the
    proof is valid. *)
val start : t -> State_hash.t

(** The state hash of the machine after the step. This must be checked
    against the value in the refutation game as well as checking the
    proof is valid. *)
val stop : t -> State_hash.t option

(** Check the validity of a proof.

    This function requires a few bits of data (available from the
    refutation game record in the storage):

      - a snapshot of the inbox, used by the [inbox] proof ;

      - the inbox level of the commitment, used to determine if an
      output from the [inbox] proof is too recent to be allowed into the
      PVM proof ;

      - the [pvm_name], used to check that the proof given has the right
      PVM kind. *)
val valid :
  Sc_rollup_inbox_repr.t ->
  Raw_level_repr.t ->
  pvm_name:string ->
  t ->
  (bool, error) result Lwt.t

module type PVM_with_context_and_state = sig
  include Sc_rollups.PVM.S

  val context : context

  val state : state
end

(** [produce pvm_and_state inbox commit_level] will construct a full
    refutation game proof out of the [state] given in [pvm_and_state].
    It uses the [inbox] if necessary to provide input in the proof. If
    the input is above or at [commit_level] it will block it, and
    produce a proof that the PVM is blocked.

    This will fail if the [context] given doesn't have enough of the
    [state] to make the proof. For example, the 'protocol
    implementation' version of each PVM won't be able to run this
    function.

    This uses the [name] in the [pvm_and_state] module to produce an
    encodable [wrapped_proof] if possible. See the [wrap_proof] function
    in [Sc_rollups]. *)
val produce :
  (module PVM_with_context_and_state) ->
  Sc_rollup_inbox_repr.t ->
  Raw_level_repr.t ->
  (t, error) result Lwt.t
