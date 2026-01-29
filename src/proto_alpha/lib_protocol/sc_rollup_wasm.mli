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

module V2_0_0 : sig
  (** This module provides Proof-Generating Virtual Machine (PVM) running
    WebAssembly (version 2.0.0). *)

  val current_version : Wasm_2_0_0.version

  module type S = sig
    include Sc_rollup_PVM_sig.S

    (** [parse_boot_sector s] builds a boot sector from its human
      writable description. *)
    val parse_boot_sector : string -> string option

    (** [pp_boot_sector fmt s] prints a human readable representation of
     a boot sector. *)
    val pp_boot_sector : Format.formatter -> string -> unit

    (* Required by L2 node: *)

    (** [get_tick state] gets the total tick counter for the given PVM state. *)
    val get_tick : state -> Sc_rollup_tick_repr.t Lwt.t

    (** PVM status *)
    type status =
      | Computing
      | Waiting_for_input_message
      | Waiting_for_reveal of Sc_rollup_PVM_sig.reveal

    (** [get_status ~is_reveal_enabled state] gives you the current execution status for the PVM. *)
    val get_status :
      is_reveal_enabled:Sc_rollup_PVM_sig.is_reveal_enabled ->
      state ->
      status Lwt.t

    (** [get_outbox outbox_level state] returns the outbox in [state]
       for a given [outbox_level]. *)
    val get_outbox :
      Raw_level_repr.t -> state -> Sc_rollup_PVM_sig.output list Lwt.t
  end

  module Make_pvm (WASM_machine : Wasm_2_0_0.WASM_PVM_MACHINE) :
    S
      with type context = WASM_machine.context
       and type state = WASM_machine.state
       and type proof = WASM_machine.proof

  (** This PVM is used for verification in the Protocol. [produce_proof] always returns [None]. *)
  module Protocol_implementation :
    S
      with type context = Wasm_2_0_0.Wasm_pvm_machine.context
       and type state = Wasm_2_0_0.Wasm_pvm_machine.state
       and type proof = Wasm_2_0_0.Wasm_pvm_machine.proof

  (** Number of ticks between snapshotable states, chosen low enough
      to maintain refutability.

      {b Warning:} This value is used to specialize the dissection
      predicate of the WASM PVM. Do not change it without a migration
      plan for already originated smart rollups.

      Depends on
      - speed (tick/s) of node in slow mode (from benchmark, 6000000 tick/s)
      - the number of ticks in a commitment ({!Int64.max_int},
         as per Number_of_ticks.max_value)

      see #3590 for more pointers *)
  val ticks_per_snapshot : Z.t

  (* The number of outboxes to keep, which is for a period of two
     weeks. For a block time of 5 seconds, this equals to (60 * 60 *
     24 * 14) / 5 = 241_920 blocks. We choose to consider 5 seconds
     instead of 10 proposed in protocol P to remove the need to
     introduce a new PVM version every time the block time is
     modified. We believe 5 seconds is small enough to be "safe" for
     multiple months. It does not create a critical issue, we will just keep
     more outboxes than expected. *)
  val outbox_validity_period : int32

  (* Maximum number of outbox messages per level.

     Equals to {Constants_parametric_repr.max_outbox_messages_per_level}. *)
  val outbox_message_limit : Z.t

  (** The hash requested by the WASM PVM if it cannot decode the input
      provided by the WASM kernel, that is, if the bytes value cannot
      be decoded with {!Sc_rollup_reveal_hash.encoding}. *)
  val well_known_reveal_hash : Sc_rollup_reveal_hash.t

  (** The preimage of {!well_known_reveal_hash}. *)
  val well_known_reveal_preimage : string

  (** Convert a raw reveal request of the WASM PVM into a typed reveal as
      defined by the protocol.

      If the decoding fails, fallback to requesting the preimage of the
      {!well_known_reveal_hash}. *)
  val decode_reveal : Wasm_2_0_0.reveal -> Sc_rollup_PVM_sig.reveal
end
