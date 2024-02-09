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

(** The proof that a reveal is valid. *)
type reveal_proof =
  | Raw_data_proof of string
      (** The existence of reveal for a given hash when the
          [input_requested] is the [Needs_reveal Reveal_raw_data]. *)
  | Metadata_proof
  | Dal_page_proof of {
      page_id : Dal_slot_repr.Page.t;
      proof : Dal_slot_repr.History.proof;
    }
      (** The existence or not of a confirmed slot for a given page ID when the
          [input_requested] is the [Needs_reveal Request_dal_page]. *)
  | Dal_parameters_proof
      (** Proof for revealing DAL parameters that were used for the slots
          published at [published_level]. The [published_level] parameter
          enables the kernel to retrieve historical DAL parameters,
          eliminating the need for each kernel to store past DAL parameters. *)

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

    - a claim that the input involved is the first input of the inbox, which
      does not need to be proved as we know by construction what is
      the input (i.e. the [Start_of_level] of the level after the rollup's
      origination level).
*)

type input_proof =
  | Inbox_proof of {
      level : Raw_level_repr.t;
      message_counter : Z.t;
      proof : Sc_rollup_inbox_repr.serialized_proof;
    }
  | Reveal_proof of reveal_proof
  | First_inbox_message

type 'proof t = {pvm_step : 'proof; input_proof : input_proof option}

type serialized = private string

(** [serialize_pvm_step ~pvm proof] turns a structured representation
    of a step proof of [pvm] into its serialized representation. *)
val serialize_pvm_step :
  pvm:('state, 'proof, 'output) Sc_rollups.PVM.implementation ->
  'proof ->
  serialized tzresult

(** [unserialize_pvm_step ~pvm proof] turns a serialized
    representation of a step proof of [pvm] into its structured
    representation. *)
val unserialize_pvm_step :
  pvm:('state, 'proof, 'output) Sc_rollups.PVM.implementation ->
  serialized ->
  'proof tzresult

type error += Sc_rollup_proof_check of string

type error += Sc_rollup_invalid_serialized_inbox_proof

val serialized_encoding : serialized Data_encoding.t

val encoding : serialized t Data_encoding.t

val pp : Format.formatter -> 'a t -> unit

(** The state hash of the machine before the step. This must be checked
    against the value in the refutation game as well as checking the
    proof is valid. *)
val start_of_pvm_step :
  pvm:('state, 'proof, 'output) Sc_rollups.PVM.implementation ->
  'proof ->
  Sc_rollup_repr.State_hash.t

(** The state hash of the machine after the step. This must be checked
    against the value in the refutation game as well as checking the
    proof is valid. *)
val stop_of_pvm_step :
  pvm:('state, 'proof, 'output) Sc_rollups.PVM.implementation ->
  'proof ->
  Sc_rollup_repr.State_hash.t

(** Check the validity of a proof.

    This function requires a few bits of data (available from the
    refutation game record in the storage):

      - a snapshot of the inbox, that may be used by the [input] proof in case
        it's an inbox message ;

      - a snapshot of the DAL confirmed slots structure, that may be used by
        the [input] proof in case the input is a DAL page ;

      - the inbox level of the commitment, used to determine if an
        output from the [input] proof is too recent to be allowed into
        the PVM proof ;

      - DAL related parameters, to be able to check the page content membership
        to a slot or check the revealed parameters if needed ;

      - the [pvm_name], used to check that the proof given has the right
        PVM kind.

      - The level at which DAL is activated (None if the DAL is not enabled).
    It also returns the optional input executed during the proof and the
    input_request for the state at the beginning of the proof.
*)
val valid :
  pvm:('state, 'proof, 'output) Sc_rollups.PVM.implementation ->
  metadata:Sc_rollup_metadata_repr.t ->
  Sc_rollup_inbox_repr.history_proof ->
  Raw_level_repr.t ->
  Dal_slot_repr.History.t ->
  Dal_slot_repr.parameters ->
  dal_activation_level:Raw_level_repr.t option ->
  dal_attestation_lag:int ->
  dal_number_of_slots:int ->
  is_reveal_enabled:Sc_rollup_PVM_sig.is_reveal_enabled ->
  'proof t ->
  (Sc_rollup_PVM_sig.input option * Sc_rollup_PVM_sig.input_request) tzresult
  Lwt.t

module type PVM_with_context_and_state = sig
  include Sc_rollups.PVM.S

  val context : context

  val state : state

  val proof_encoding : proof Data_encoding.t

  val reveal : Sc_rollup_reveal_hash.t -> string option Lwt.t

  module Inbox_with_history : sig
    val inbox : Sc_rollup_inbox_repr.history_proof

    val get_history :
      Sc_rollup_inbox_repr.Hash.t ->
      Sc_rollup_inbox_repr.history_proof option Lwt.t

    val get_payloads_history :
      Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t ->
      Sc_rollup_inbox_merkelized_payload_hashes_repr.History.t Lwt.t
  end

  (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3997
     This interface might not be resilient to dal parameters changes
     (cryptobox parameters or dal_attestation_lag for instance). *)
  module Dal_with_history : sig
    (** The reference/snapshot cell of the DAL skip list that stores
        confirmed slots. *)
    val confirmed_slots_history : Dal_slot_repr.History.t

    (** A function to retrieve a history from an underlying cache. *)
    val get_history :
      Dal_slot_repr.History.hash -> Dal_slot_repr.History.t option Lwt.t

    (** In case we expect to generate an input proof that is a DAL page
        confirmation, we should provide via [page_info] the information of the
        page. That is: its content and the proof that the page is part of a
        confirmed slot whose ID is part of the page's ID.

        In case we expect to generate an input proof to justify that a DAL page
        is not confirmed, the value of [page_info] should be [None].

        In case the proof doesn't involve DAL inputs, the value of [page_info]
        is [None]. *)
    val page_info :
      (Dal_slot_repr.Page.content * Dal_slot_repr.Page.proof) option

    (** Some parameters of the DAL. Needed when checking a page's proof against
        a slot's {!val: Dal_slot_repr.commitment}. *)
    val dal_parameters : Dal_slot_repr.parameters

    (** The lag between the time an attestation is published on L1
        (its published_level) and the level it should be confirmed. *)
    val dal_attestation_lag : int

    (** The number of DAL slots provided by the L1. *)
    val dal_number_of_slots : int

    (** The level at which the DAL got activated, [None] if the DAL has not yet been activated. *)
    val dal_activation_level : Raw_level_repr.t option
  end
end

(** [produce ~metadata pvm_and_state inbox_context inbox_history
    commit_inbox_level] will construct a full refutation game proof out of
    the [state] given in [pvm_and_state].  It uses the [inbox] if necessary to
    provide input in the proof. If the input is above or at [commit_level] it
    will block it, and produce a proof that the PVM is blocked. If
    the input requested is a reveal the proof production will also
    fail.

    This will fail if any of the [context], [inbox_context], [inbox_history] or
    [dal_slots_history_cache] given doesn't have enough data to make the proof.
    For example, the 'protocol implementation' version of each PVM won't be
    able to run this function. Similarly, the version of the inbox
    stored in the L1 won't be enough because it forgets old levels.

    This uses the [name] in the [pvm_and_state] module to produce an
    encodable [wrapped_proof] if possible. See the [wrap_proof] function
    in [Sc_rollups].

    It also need the [metadata] if it produces a proof for the [Needs_metadata]
    state.
*)
val produce :
  metadata:Sc_rollup_metadata_repr.t ->
  (module PVM_with_context_and_state) ->
  Raw_level_repr.t ->
  is_reveal_enabled:Sc_rollup_PVM_sig.is_reveal_enabled ->
  serialized t tzresult Lwt.t

module Dal_helpers : sig
  (** We consider that a DAL page or slot published at a level [published_level]
      is valid, and produce or verify a proof for it if, and only if, the level
      is in the following boundaries:

      - DAL is activated and [published_level] >= [dal_activation_level]
      - [published_level] > [origination_level]: this means that the slot of the
      page was published after the rollup origination ;

      - [published_level] + [dal_attestation_lag] <= [commit_inbox_level]: this
      means that the slot of the page has been attested before or at the
      [commit_inbox_level].

      According to the definition in {!Sc_rollup_commitment_repr},
      [commit_inbox_level] (aka inbox_level in that module) is the level
      (included) up to which the PVM consumed all messages and DAL/DAC inputs
      before producing the related commitment.
      We also check that the given slot ID's index is within the range of
      allowed slots thanks to [dal_number_of_slots].  *)
  val valid_slot_id :
    dal_number_of_slots:int ->
    dal_activation_level:Raw_level_repr.t option ->
    dal_attestation_lag:int ->
    origination_level:Raw_level_repr.t ->
    commit_inbox_level:Raw_level_repr.t ->
    Dal_slot_repr.Header.id ->
    bool
end

(**/**)

module Internal_for_tests : sig
  (** Export internal [cut_at_level] function. See the docstring in the
      implementation file for more information. *)
  val cut_at_level :
    origination_level:Raw_level_repr.t ->
    commit_inbox_level:Raw_level_repr.t ->
    Sc_rollup_PVM_sig.input ->
    Sc_rollup_PVM_sig.input option
end
