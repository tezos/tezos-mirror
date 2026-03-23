(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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

type version

val v6 : version

type input = {inbox_level : Bounded.Non_negative_int32.t; message_counter : Z.t}

type output = {outbox_level : Bounded.Non_negative_int32.t; message_index : Z.t}

type reveal = Reveal_raw of string

type input_request =
  | No_input_required
  | Input_required
  | Reveal_required of reveal

type info = {
  current_tick : Z.t;
  last_input_read : input option;
  input_request : input_request;
}

module type WASM_PVM_MACHINE = sig
  type state

  val initial_state : version -> state -> state Lwt.t

  val install_boot_sector :
    ticks_per_snapshot:Z.t ->
    outbox_validity_period:int32 ->
    outbox_message_limit:Z.t ->
    string ->
    state ->
    state Lwt.t

  val compute_step : state -> state Lwt.t

  val set_input_step : input -> string -> state -> state Lwt.t

  val reveal_step : bytes -> state -> state Lwt.t

  val get_output : output -> state -> string option Lwt.t

  val get_info : state -> info Lwt.t

  type context

  val empty_state : unit -> state

  type proof

  val state_hash : state -> Smart_rollup.State_hash.t Lwt.t

  val proof_encoding : proof Data_encoding.t

  val proof_start_state : proof -> Smart_rollup.State_hash.t

  val proof_stop_state : proof -> Smart_rollup.State_hash.t

  val cast_read_only : proof -> proof

  val verify_proof :
    proof -> (state -> (state * 'a) Lwt.t) -> (state * 'a) option Lwt.t

  module Internal_for_tests : sig
    val insert_failure : state -> state Lwt.t
  end
end

module Wasm_pvm_machine : WASM_PVM_MACHINE
