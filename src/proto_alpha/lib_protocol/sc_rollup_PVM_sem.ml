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

open Sc_rollup_repr

(** An input to a PVM is the [message_counter] element of an inbox at
    a given [inbox_level] and contains a given [payload]. *)
type input = {
  inbox_level : Raw_level_repr.t;
  message_counter : Z.t;
  payload : string;
}

let input_encoding =
  let open Data_encoding in
  conv
    (fun {inbox_level; message_counter; payload} ->
      (inbox_level, message_counter, payload))
    (fun (inbox_level, message_counter, payload) ->
      {inbox_level; message_counter; payload})
    (obj3
       (req "inbox_level" Raw_level_repr.encoding)
       (req "message_counter" n)
       (req "payload" string))

let input_equal (a : input) (b : input) : bool =
  let {inbox_level; message_counter; payload} = a in
  (* To be robust to the addition of fields in [input] *)
  Raw_level_repr.equal inbox_level b.inbox_level
  && Z.equal message_counter b.message_counter
  && String.equal payload b.payload

(** The PVM's current input expectations. [No_input_required] is if the
    machine is busy and has no need for new input. [Initial] will be if
    the machine has never received an input so expects the very first
    item in the inbox. [First_after (level, counter)] will expect
    whatever comes next after that position in the inbox. *)
type input_request =
  | No_input_required
  | Initial
  | First_after of Raw_level_repr.t * Z.t

let input_request_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"No_input_required"
        (Tag 0)
        (obj1 (req "no_input_required" unit))
        (function No_input_required -> Some () | _ -> None)
        (fun () -> No_input_required);
      case
        ~title:"Initial"
        (Tag 1)
        (obj1 (req "initial" unit))
        (function Initial -> Some () | _ -> None)
        (fun () -> Initial);
      case
        ~title:"First_after"
        (Tag 2)
        (tup2 Raw_level_repr.encoding n)
        (function
          | First_after (level, counter) -> Some (level, counter) | _ -> None)
        (fun (level, counter) -> First_after (level, counter));
    ]

let pp_input_request fmt request =
  match request with
  | No_input_required -> Format.fprintf fmt "No_input_required"
  | Initial -> Format.fprintf fmt "Initial"
  | First_after (l, n) ->
      Format.fprintf
        fmt
        "First_after (level = %a, counter = %a)"
        Raw_level_repr.pp
        l
        Z.pp_print
        n

let input_request_equal a b =
  match (a, b) with
  | No_input_required, No_input_required -> true
  | No_input_required, _ -> false
  | Initial, Initial -> true
  | Initial, _ -> false
  | First_after (l, n), First_after (m, o) ->
      Raw_level_repr.equal l m && Z.equal n o
  | First_after _, _ -> false

type output = {
  outbox_level : Raw_level_repr.t;
  message_index : Z.t;
  message : Sc_rollup_outbox_message_repr.t;
}

let output_encoding =
  let open Data_encoding in
  conv
    (fun {outbox_level; message_index; message} ->
      (outbox_level, message_index, message))
    (fun (outbox_level, message_index, message) ->
      {outbox_level; message_index; message})
    (obj3
       (req "outbox_level" Raw_level_repr.encoding)
       (req "message_index" n)
       (req "message" Sc_rollup_outbox_message_repr.encoding))

let pp_output fmt {outbox_level; message_index; message} =
  Format.fprintf
    fmt
    "@[%a@;%a@;%a@;@]"
    Raw_level_repr.pp
    outbox_level
    Z.pp_print
    message_index
    Sc_rollup_outbox_message_repr.pp
    message

module type S = sig
  (**

       The state of the PVM denotes a state of the rollup.

       We classify states into two categories: "internal states" do
       not require any external information to be executed while
       "input states" are waiting for some information from the
       inbox to be executable.

  *)
  type state

  (** A state is initialized in a given context. A [context]
      represents the executable environment needed by the state to
      exist. Typically, the rollup node storage can be part of this
      context to allow the PVM state to be persistent. *)
  type context

  (** A [hash] characterizes the contents of a state. *)
  type hash = State_hash.t

  (** During interactive refutation games, a player may need to
      provide a proof that a given execution step is valid. The PVM
      implementation is responsible for ensuring that this proof type
      has the correct semantics:

        A proof [p] has four parameters:

          [start_hash := proof_start_state p]
          [stop_hash := proof_stop_state p]
          [input_requested := proof_input_requested p]
          [input_given := proof_input_given p]

        The following predicate must hold of a valid proof:

          [exists start_state, stop_state.
               (state_hash start_state == start_hash)
           AND (Option.map state_hash stop_state == stop_hash)
           AND (is_input_state start_state == input_requested)
           AND (match (input_given, input_requested) with
                | (None, No_input_required) -> eval start_state == stop_state
                | (None, Initial) -> stop_state == None
                | (None, First_after (l, n)) -> stop_state == None
                | (Some input, No_input_required) -> true
                | (Some input, Initial) ->
                    set_input input_given start_state == stop_state
                | (Some input, First_after (l, n)) ->
                    set_input input_given start_state == stop_state)]

      In natural language---the two hash parameters [start_hash] and
      [stop_hash] must have actual [state] values (or possibly [None] in
      the case of [stop_hash]) of which they are the hashes. The
      [input_requested] parameter must be the correct request from the
      [start_hash], given according to [is_input_state]. Finally there
      are four possibilities of [input_requested] and [input_given].

      - if no input is required, or given, the proof is a simple [eval]
        step ;
      - if input was required but not given, the [stop_hash] must be
        [None] (the machine is blocked) ;
      - if no input was required but some was given, this makes no sense
        and it doesn't matter if the proof is valid or invalid (this
        case will be ruled out by the inbox proof anyway) ;
      - finally, if input was required and given, the proof is a
        [set_input] step. *)
  type proof

  (** [proof]s are embedded in L1 refutation game operations using
      [proof_encoding]. Given that the size of L1 operations are
      limited, it is of *critical* importance to make sure that
      no execution step of the PVM can generate proofs that do not
      fit in L1 operations when encoded. If such a proof existed,
      the rollup could get stuck. *)
  val proof_encoding : proof Data_encoding.t

  (** [proof_start_state proof] returns the initial state hash of the
      [proof] execution step. *)
  val proof_start_state : proof -> hash

  (** [proof_stop_state proof] returns the final state hash of the
      [proof] execution step. *)
  val proof_stop_state : proof -> hash option

  (** [proof_input_requested proof] returns the [input_request] status
      of the start state of the proof, as given by [is_input_state].
      This must match with the inbox proof to complete a valid
      refutation game proof. *)
  val proof_input_requested : proof -> input_request

  (** [proof_input_given proof] returns the [input], if any, provided to
      the start state of the proof using the [set_input] function. If
      [None], the proof is an [eval] step instead, or the machine is
      blocked because the inbox is fully read. This must match with the
      inbox proof to complete a valid refutation game proof. *)
  val proof_input_given : proof -> input option

  (** [state_hash state] returns a compressed representation of [state]. *)
  val state_hash : state -> hash Lwt.t

  (** [initial_state context boot_sector] is the initial state of the PVM,
      which is a pure function of [boot_sector].

      The [context] argument is required for technical reasons and does
      not impact the result. *)
  val initial_state : context -> string -> state Lwt.t

  (** [is_input_state state] returns the input expectations of the
      [state]---does it need input, and if so, how far through the inbox
      has it read so far? *)
  val is_input_state : state -> input_request Lwt.t

  (** [set_input (level, n, msg) state] sets [msg] in [state] as
      the next message to be processed. This input message is assumed
      to be the number [n] in the inbox messages at the given
      [level]. The input message must be the message next to the
      previous message processed by the rollup. *)
  val set_input : input -> state -> state Lwt.t

  (** [eval s0] returns a state [s1] resulting from the
      execution of an atomic step of the rollup at state [s0]. *)
  val eval : state -> state Lwt.t

  (** This checks the proof. See the doc-string for the [proof] type. *)
  val verify_proof : proof -> bool Lwt.t

  (** [produce_proof ctxt input_given state] should return a [proof] for
      the PVM step starting from [state], if possible. This may fail for
      a few reasons:
        - the [input_given] doesn't match the expectations of [state] ;
        - the [context] for this instance of the PVM doesn't have access
        to enough of the [state] to build the proof. *)
  val produce_proof :
    context -> input option -> state -> (proof, error) result Lwt.t

  (** The following type is inhabited by the proofs that a given [output]
      is part of the outbox of a given [state]. *)
  type output_proof

  (** [output_proof_encoding] encoding value for [output_proof]s. *)
  val output_proof_encoding : output_proof Data_encoding.t

  (** [output_of_output_proof proof] returns the [output] that is
      referred to in [proof]'s statement. *)
  val output_of_output_proof : output_proof -> output

  (** [state_of_output_proof proof] returns the [state] hash that is
      referred to in [proof]'s statement. *)
  val state_of_output_proof : output_proof -> hash

  (** [verify_output_proof output_proof] returns [true] iff [proof] is
      a valid witness that its [output] is part of its [state]'s outbox. *)
  val verify_output_proof : output_proof -> bool Lwt.t

  (** [produce_output_proof ctxt state output] returns a proof
      that witnesses the fact that [output] is part of [state]'s
      outbox. *)
  val produce_output_proof :
    context -> state -> output -> (output_proof, error) result Lwt.t
end
