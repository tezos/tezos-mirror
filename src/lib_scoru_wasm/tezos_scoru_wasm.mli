(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** This module exposes a module type {!S} defining a WASM VM API.
    It also exposes a functor {!Make} for constructing a concrete implementation
    of this module type, given an implementation of a {!Tree.S} module.

    This library acts as a dependency to the protocol environment. Everything
    WASM VM related that must be exposed to the protocol via the environment
    shall be added here. *)

(** Represents the location of an input message. *)
type input = {
  inbox_level : Tezos_base.Bounded.Int32.NonNegative.t;
      (** The inbox level at which the message exists.*)
  message_counter : Z.t;  (** The index of the message in the inbox. *)
}

(** Represents the location of an output message. *)
type output = {
  outbox_level : Tezos_base.Bounded.Int32.NonNegative.t;
      (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

(** Represents the state of input requests. *)
type input_request =
  | No_input_required  (** The VM does not expect any input. *)
  | Input_required  (** The VM needs input in order to progress. *)

(** Represents the state of the VM. *)
type info = {
  current_tick : Z.t;
      (** The number of ticks processed by the VM, zero for the initial state.
      [current_tick] must be incremented for each call to [step] *)
  last_input_read : input option;
      (** The last message to be read by the VM, if any. *)
  input_request : input_request;  (** The current VM input request. *)
}

(** This module type defines a WASM VM API used for smart-contract rollups. *)
module type S = sig
  type tree

  (** [compute_step] forwards the VM by one compute tick. If the VM is expecting
      input, it gets stuck. If the VM is already stuck, this function may
      raise an exception. *)
  val compute_step : tree -> tree Lwt.t

  (** [set_input_step] forwards the VM by one input tick. If the VM is not
      expecting input, it gets stuck. If the VM is already stuck, this function
      may raise an exception. *)
  val set_input_step : input -> string -> tree -> tree Lwt.t

  (** [get_output output state] returns the payload associated with the given
      output. The result is meant to be deserialized using
      [Sc_rollup_PVM_sem.output_encoding]. If the output is missing, this
      function may raise an exception. *)
  val get_output : output -> tree -> string Lwt.t

  (** [get_info] provides a typed view of the current machine state. Should not
      raise. *)
  val get_info : tree -> info Lwt.t
end

(** Builds a WASM VM given a concrete implementation of {!Tree.S}. *)
module Make (T : Tree.S) : S with type tree = T.tree

exception Bad_input

(** [aux_write_memory ~input_buffer ~module_inst ~rtype_offset ~level_offset 
     ~id_offset ~dst ~max_bytes]
     reads `input_buffer` and writes its components to the  memory of 
    `module_inst` based on the memory addreses offsets described. It also 
    checks that the input payload is no larger than `max_input` and crashes 
    with `input too large` otherwise. It returns the size of the payload.*)
val aux_write_input_in_memory :
  input_buffer:Tezos_webassembly_interpreter.Input_buffer.t ->
  module_inst:Tezos_webassembly_interpreter.Instance.module_inst ref ->
  rtype_offset:int64 ->
  level_offset:int64 ->
  id_offset:int64 ->
  dst:int64 ->
  max_bytes:int64 ->
  int Lwt.t

(** read_input is a HostFunction. It has to be invoked with a list of 5 values
  representing rtype_offset, level_offset, id_offset, dst and max_bytes. When 
  invoked, it applies `aux_write_input_in_memory` with the corresponding 
  parameters and returns a singleton value list containing the size of the 
  input_buffer payload.   *)
val read_input :
  ( Tezos_webassembly_interpreter.Input_buffer.t,
    Tezos_webassembly_interpreter.Instance.module_inst ref )
  Tezos_webassembly_interpreter.Func.func
